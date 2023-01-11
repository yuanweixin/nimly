import tables
import hashes
import sets

import patty

import parsetypes
import parser
import std/options
import debuginfo

proc initTransTableRow[T](): Table[Symbol[T], int] =
  result = initTable[Symbol[T], int]()

proc indexOf[T](os: OrderedSet[T], element: T): int =
  for i, key in os:
    if key == element:
      return i
  return -1

proc pointForward[T](i: LRItem[T]): LRItem[T] =
  ## move dot up by 1 
  doAssert i.pos < i.rule.len
  result = LRItem[T](rule: i.rule, pos: i.pos + 1)

proc closure[T](g: Grammar[T], whole: LRItems[T]): LRItems[T] =
  result = whole
  var checkSet = whole
  while checkSet.len > 0:
    var new: LRItems[T]
    for i in checkSet:
      match i.next:
        # S -> p.Nq where N is NonTerm 
        NonTermS:
          # look for N -> ...
          for r in g.filterRulesLeftIs(i.next):
            let n = LRItem[T](rule: r, pos: 0)
            if not result.containsOrIncl(n):
              new.incl(n)
        _:
          discard
    checkSet = new

proc goto[T](g: Grammar[T], itms: LRItems[T], s: Symbol[T]): LRItems[T] =
  doAssert s.kind != SymbolKind.End
  assert itms == g.closure(itms)
  var gotoHashSet = initHashSet[LRItem[T]]()
  for i in itms:
    if i.next == s:
      gotoHashSet.incl(i.pointForward)
  result = g.closure(gotoHashSet)


proc resolveShiftReduceConflict*[T](r: Rule[T], t: T, g: Grammar[T], state: State): ActionTableItem[T] = 
  let rp = g.getPrecedence(r)
  let tp = g.getPrecedence($t)
  if rp.isSome and tp.isSome:
    if tp.get > rp.get:
      return Shift[T](state)
    elif rp.get > tp.get:
      return Reduce[T](r)
    else:
      let assoc = g.getAssociativity($t)
      doAssert assoc.isSome, "Bug in implementation: rule's precedence token must exist in grammar declaration's associativity declarations, and since it has same precedence as the lookahead, the lookahead must belong to the same line of associativity declaration. But no associativity exists."
      case assoc.get 
      of Left:
        return Reduce[T](r)
      of Right:
        return Shift[T](state)
      of NonAssoc:
        return Error[T]()
  return Shift[T](state)

proc makeCanonicalCollection*[T](g: Grammar[T]): (SetOfLRItems[T],
                                                  TransTable[T]) =
  # seed C with the closure of start rule
  let init = g.closure([LRItem[T](rule: g.startRule, pos: 0)].toHashSet)
  var
    cc = [
      init
    ].toOrderedSet
    checkSet = cc
    tt: TransTable[T] = @[]
  tt.add(initTransTableRow[T]())
  while checkSet.len > 0:
    var new: SetOfLRItems[T]
    # for each set of items I in C 
    for itms in checkSet:
      let frm = cc.indexOf(itms)
      assert itms == g.closure(itms)
      for i in itms:
        # for each (applicable) grammar symbol X 
        let s = i.next
        if s != End[T]():
          let gt = goto[T](g, itms, s)
          # if GOTO(I,X) exist (it must since we use i.next) and is not in C 
          if (not cc.containsOrIncl(gt)):
            # Add GOTO(I,X) to C 
            tt.add(initTransTableRow[T]())
            assert cc.card == tt.len
            new.incl(gt)
          tt[frm][s] = cc.indexOf(gt)
    checkSet = new
  doAssert cc.indexOf(init) == 0, "init state is not '0'"
  result = (cc, tt)

proc makeTableSLR*[T](g: Grammar[T]): ParsingTable[T] =
  ## This produces the SLR table. 
  var
    actionTable: ActionTable[T]
    gotoTable: GotoTable[T]
  actionTable = initTable[State, ActionRow[T]]()
  gotoTable = initTable[State, GotoRow[T]]()
  let
    ag = if g.isAugment:
           g
         else:
           g.augment
    (canonicalCollection, _) = makeCanonicalCollection[T](ag)
  var cntSR = 0 
  var cntRR = 0 
  for idx, itms in canonicalCollection:
    actionTable[idx] = initTable[Symbol[T], ActionTableItem[T]]()
    gotoTable[idx] = initTable[Symbol[T], State]()
    for item in itms:
      let sym = item.nextSkipEmpty
      match sym:
        TermS: # shift terminals
          let i = canonicalCollection.indexOf(ag.goto(itms, sym))
          assert i > -1,"There is no 'items' which is equal to 'goto'"

          if actionTable[idx].haskey(sym) and
            actionTable[idx][sym].kind == ActionTableItemKind.Reduce:
            echo "SLR:Shift-Reduce CONFLICT!!!" & $idx & ":" & $sym
            actionTable[idx][sym] = resolveShiftReduceConflict(actionTable[idx][sym].rule, sym.term, g, i)
            echo "Conflict resolved in favor of " & $actionTable[idx][sym]
            inc cntSR 
          elif actionTable[idx].haskey(sym) and
            actionTable[idx][sym].kind == ActionTableItemKind.Error:
            continue
          else:
            actionTable[idx][sym] = Shift[T](i)
        NonTermS: # goto nonterminals 
          let i = canonicalCollection.indexOf(ag.goto(itms, sym))
          assert i > -1, "There is no 'items' which is equal to 'goto'"
          gotoTable[idx][sym] = i
        End: # accept or reduce 
          if item.rule.left == ag.start:
            actionTable[idx][End[T]()] = Accept[T]()
          else:
            # this is the SLR reduction rule: give A -> a., only reduce 
            # if input is in FOLLOW(A)
            for flw in ag.followTable[item.rule.left]:
              if actionTable[idx].haskey(flw) and
                  actionTable[idx][flw].kind == ActionTableItemKind.Shift:
                # we cannot shift the End symbol, so this has to be TermS
                doAssert flw.kind == SymbolKind.TermS, "bug, Shift(End) is not possible"
                echo "SLR:Shift-Reduce CONFLICT!!!" & $idx & ":" & $flw
                actionTable[idx][flw] = resolveShiftReduceConflict(item.rule, flw.term, g, actionTable[idx][flw].state)
                echo "Conflict resolved in favor of " & $actionTable[idx][flw]
                inc cntSR 
              elif actionTable[idx].haskey(flw) and
                  actionTable[idx][flw].kind == ActionTableItemKind.Reduce:
                echo "SLR:Reduce-Reduce CONFLICT!!!" & $idx & ":" & $flw & ". This usually indicates a serious error in the grammar. Try unfactoring grammar to eliminate the conflict. As is, the first rule to get processed wins."
                inc cntRR
              elif actionTable[idx].haskey(sym) and
                actionTable[idx][sym].kind == ActionTableItemKind.Error:
                continue
              else:
                actionTable[idx][flw] = Reduce[T](item.rule)
        _:
          when defined(nimydebug):
            echo "SLR: OTHER (" & $sym & ")"
          discard
  result = ParsingTable[T](action: actionTable, goto: gotoTable)
  when defined(nimydebug):
    echo "\n\nDone making SLR tables"
    echo "Debug info:"
    echo $g
    echo $cntSR & " shift-reduce conflict(s)"
    echo $cntRR & " reduce-reduce conflict(s)"
    echo "=============================="
    echo "SLR automaton"
    for idx, itms in canonicalCollection:
      echo "state " & $idx 
      echo lrItemsToString(itms, g, actionTable, gotoTable, idx)
      echo "=============================="
    echo actionTable
    echo gotoTable

proc filterKernel*[T](cc: SetOfLRItems[T]): SetOfLRItems[T] =
  result = initOrderedSet[LRItems[T]]()
  let start = NonTermS[T]("__Start__")
  for i, itms in cc:
    for itm in itms:
      var kernelItems = initHashSet[LRItem[T]]()
      for itm in itms:
        if itm.pos != 0 or itm.rule.left == start:
          kernelItems.incl(itm)
      result.incl(kernelItems)
