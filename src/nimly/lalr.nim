import tables
import sets
import hashes

import patty

import parsetypes
import parser
import slr
import std/options
import std/strutils
import debuginfo
import dotted

proc initLALRItems[T](): LALRItems[T] =
  result = initHashSet[LALRItem[T]]()

proc initHashSetOfLALRItems[T](): SetOfLALRItems[T] =
  result = initOrderedTable[int, LALRItems[T]]()

proc initPropagateTable[T](): PropagateTable[T] =
  result = initTable[LRItem[T], HashSet[(int, LRItem[T])]]()

proc fromNextNext[T](i: LALRItem[T]): seq[Symbol[T]] =
  result = @[]
  doAssert i.pos < i.rule.len
  for index in (i.pos + 1)..<i.rule.len:
    result.add(i.rule.right[index])

proc closure[T](g: Grammar[T], whole: LALRItems[T]): LALRItems[T] =
  result = whole
  var checkSet = whole
  while checkSet.len > 0:
    var new: LALRItems[T]
    for i in checkSet:
      match i.next:
        NonTermS:
          for r in g.filterRulesLeftIs(i.next):
            doAssert i.ahead.kind != SymbolKind.Empty, "Lookahead is EMPTY (epsilon)"
            for fst in g.calFirsts(i.fromNextNext & i.ahead):
              # for each terminal b in FIRST(βa), dragonbook fig. 4.40
              if fst.kind == SymbolKind.Empty:
                continue 
              let n = LALRItem[T](rule: r, pos: 0, ahead: fst)
              if not result.containsOrIncl(n):
                new.incl(n)
        _:
          discard
    checkSet = new

proc closure[T](g: Grammar[T], single: LALRItem[T]): LALRItems[T] =
  result = g.closure([single].toHashSet)

proc toLALRItem[T](lrItem: LRItem[T], ahead: Symbol[T]): LALRItem[T] =
  result = LALRItem[T](rule: lrItem.rule, pos: lrItem.pos, ahead: ahead)


proc `[]`[T](pt: PropagateTable[T],
             itm: LALRItem[T]): HashSet[(int, LRItem[T])] =
  result = pt[LRItem[T](rule: itm.rule, pos: itm.pos)]

proc incl[T](ot: var OrderedTable[int, T], vl: T) =
  ot[ot.len] = vl

proc forward[T](itm: LALRItem[T]): LALRItem[T] =
  result = LALRItem[T](rule: itm.rule, pos: itm.pos + 1, ahead: itm.ahead)

proc firstItem[T](os: OrderedSet[T]): T =
  for i in os:
    return i

proc getItemIfSingle[T](s: HashSet[T]): T =
  if s.card == 1:
    for i in s:
      return i
  raise newException(NimyError, "Unexpected: " & $s & " needs to be single.")

## Same as Dragonbook Argorithm 4.62 & 4.63
proc toLALRKernel[T](lrKernel: SetOfLRItems[T], g: Grammar[T],
                     tt: TransTable[T]): SetOfLALRItems[T] =
  # init result
  result = initHashSetOfLALRItems[T]()
  doAssert lrKernel.card > 0
  for idx in 0..<lrKernel.card:
    result.incl(initLALRItems[T]())
  var
    propagation: PropagateTable[T] = initPropagateTable[T]()
    checkSet: HashSet[LALRItem[T]] = initLALRItems[T]()

  # only starting rule
  let startingRule = lrKernel.firstItem.getItemIfSingle
  result[0].incl(startingRule.toLALRItem(End[T]()))
  checkSet.incl(startingRule.toLALRItem(End[T]()))

  # init collection and cal propagate
  for idx, itms in lrKernel:
    when defined(nimydebug):
      echo "[nimly] converting kernel: " & $(idx + 1) & "/" & $lrKernel.len
    for itm in itms:
      if not (propagation.haskey(itm)):
        propagation[itm] = initHashSet[(int, LRItem[T])]()

      # Dummy is "#" in dragonbook
      let clsr = g.closure(itm.toLALRItem(Dummy[T]()))
      for ci in clsr:
        if ci.ahead == Dummy[T]():
          if ci.next != End[T]():
            propagation[itm] = (propagation[itm] +
                                [(tt[idx][ci.next],
                                  ci.forward.toLRItem)].toHashSet)
        else:
          let prpgtd = ci.forward
          assert tt[idx][ci.next] < lrKernel.card
          result[tt[idx][ci.next]].incl(prpgtd)
          checkSet.incl(prpgtd)

  # cal collection
  while checkSet.card > 0:
    var newSet = initLALRItems[T]()
    for itm in checkSet:
      # propagation[itm] where itm is a LALRItem works because we 
      # snuck in a `[]` proc above, very sneaky and confusing. 
      for toInfo in propagation[itm]: 
        let
          (idx, toItm) = toInfo
          new = toItm.toLALRItem(itm.ahead)
        if not (result[idx].containsOrIncl(new)):
          newSet.incl(new)
    checkSet = newSet

proc makeTableLALR*[T](g: Grammar[T]): ParsingTable[T] =
  var
    actionTable: ActionTable[T]
    gotoTable: GotoTable[T]
  actionTable = initTable[State, ActionRow[T]]()
  gotoTable = initTable[State, GotoRow[T]]()
  when defined(nimydebug):
    echo "[nimly] start: make table for parser"
  let
    ag = if g.isAugment:
           g
         else:
           g.augment
    (cc, tt) = makeCanonicalCollection[T](ag)
    knl = cc.filterKernel
    lalrKnl = knl.toLALRKernel(ag, tt)
  var cntSR = 0 
  var cntRR = 0 
  for idx, itms in lalrKnl:
    when defined(nimydebug):
      echo "[nimly] processing: Collection " & $(idx + 1) & "/" & $lalrKnl.len
    actionTable[idx] = initTable[Symbol[T], ActionTableItem[T]]()
    gotoTable[idx] = initTable[Symbol[T], State]()
    when defined(nimydebug):
      echo "[nimly] processing: Collection " & $(idx + 1) & " - make closure"
    let clsr = ag.closure(itms)
    var cnt = 1
    for itm in clsr:
      when defined(nimydebug):
        echo "[nimly] processing: Collection " & $(idx + 1) & " - " &
          $cnt & "/" & $clsr.card
      inc(cnt)
      let sym = itm.nextSkipEmpty
      match sym:
        ErrorS:
          actionTable[idx][sym] = Shift[T](tt[idx][sym])
        TermS:
          if actionTable[idx].haskey(sym) and
              actionTable[idx][sym].kind == ActionTableItemKind.Reduce:
            actionTable[idx][sym] = resolveShiftReduceConflict(actionTable[idx][sym].rule, sym.term, g, tt[idx][sym])
            when defined(nimyDebug):
              echo "LALR:Shift-Reduce CONFLICT!!!" & $idx & ":" & $sym 
              echo "Resolved in favor of " & $actionTable[idx][sym]
            inc cntSR 
          elif actionTable[idx].haskey(sym) and
              actionTable[idx][sym].kind == ActionTableItemKind.Error:
            continue 
          else:
            actionTable[idx][sym] = Shift[T](tt[idx][sym])
        NonTermS:
          gotoTable[idx][sym] = tt[idx][sym]
        End:
          if itm.rule.left == ag.start:
            actionTable[idx][End[T]()] = Accept[T]()
          else:
            if actionTable[idx].haskey(itm.ahead) and
               actionTable[idx][itm.ahead].kind == ActionTableItemKind.Shift:
              actionTable[idx][itm.ahead] = resolveShiftReduceConflict(itm.rule, 
              itm.ahead.term, g, actionTable[idx][itm.ahead].state)
              when defined(nimydebug):
                echo "LALR:Shift-Reduce CONFLICT!!!" & $idx & ":" & $itm.ahead
                echo "Conflict resolved in favor of " & $actionTable[idx][itm.ahead]
              inc cntSR 
            elif actionTable[idx].haskey(itm.ahead) and
               actionTable[idx][itm.ahead].kind == ActionTableItemKind.Reduce: 
              echo "LALR:Reduce-Reduce CONFLICT!!!" & $idx & ":" & $itm.ahead & ".  This usually indicates a serious error in the grammar. It could also be due to the LALR table compression, where multiple reducible rules are placed into the same parser state and there is insufficient context to distinguish them. A possible solution is to add a bogus token to one of the rules to force it into a distinct parser state. Another possible solution is to rewrite the grammar rules to reduce ambiguity." 
              inc cntRR
              continue
            elif actionTable[idx].haskey(itm.ahead) and
               actionTable[idx][itm.ahead].kind == ActionTableItemKind.Error:
              continue
            else:
              actionTable[idx][itm.ahead] = Reduce[T](itm.rule)
        _:
          discard

  when defined(nimydebug):
    echo "\n\nDone making LALR tables"
    echo "Debug info:"
    echo $g
    echo $cntSR & " shift-reduce conflict(s)"
    echo $cntRR & " reduce-reduce conflict(s)"
    echo "=============================="
    echo "LALR automaton"
    for idx, itms in lalrKnl:
      echo "state " & $idx 
      let clj = ag.closure(itms)
      echo lalrItemsToString(clj, g, actionTable, gotoTable, idx)
      echo "=============================="
    echo actionTable 
    echo gotoTable

  when defined(nimygraphviz):
    var dg = emptyDotGraph()
    # repeat closure calculation in nimydebug...
    # oh well. 
    for idx, itms in lalrKnl:
      let clj = ag.closure(itms)
      populateDotGraph(dg, clj, g, actionTable, gotoTable, idx)
    echo dg.render    

  result = ParsingTable[T](action: actionTable, goto: gotoTable)
