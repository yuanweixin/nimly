import tables
import sets
import hashes

import patty

import parsetypes
import slr
import options
import debuginfo
import dotted
import dev_assert


proc initLALRItems(): LALRItems =
  result = initHashSet[LALRItem]()

proc initPropagateTable(): PropagateTable =
  result = initTable[LRItem, HashSet[(int, LRItem)]]()

proc closure(g: Grammar, whole: LALRItems): LALRItems =
  result = whole
  var checkSet = whole
  var new: LALRItems
  while checkSet.len > 0:
    for i in checkSet:
      match i.next:
        NonTermS:
          for r in g.filterRulesLeftIs(i.next):
            nimyaccAssert i.ahead.kind != SymbolKind.Empty, "Lookahead is EMPTY (epsilon)"
            for fst in g.calFirsts(i):
              # for each terminal b in FIRST(βa), dragonbook fig. 4.40
              if fst.kind == SymbolKind.Empty:
                continue 
              let n = LALRItem(ruleIdx: r.index, pos: 0, ahead: fst, g: g)
              if not result.containsOrIncl(n):
                new.incl(n)
        _:
          discard
    checkSet = new
    new.clear()

proc closure(g: Grammar, single: LALRItem): LALRItems =
  result = g.closure([single].toHashSet)

proc toLALRItem(lrItem: LRItem, ahead: Symbol): LALRItem =
  result = LALRItem(ruleIdx: lrItem.ruleIdx, pos: lrItem.pos, ahead: ahead, g: lrItem.g)


proc `[]`(pt: PropagateTable,
             itm: LALRItem): HashSet[(int, LRItem)] =
  result = pt[LRItem(ruleIdx: itm.ruleIdx, pos: itm.pos, g: itm.g)]

proc incl[T](ot: var OrderedTable[int, T], vl: T) =
  ot[ot.len] = vl

proc forward(itm: LALRItem): LALRItem =
  result = LALRItem(ruleIdx: itm.ruleIdx, pos: itm.pos + 1, ahead: itm.ahead, g: itm.g)

proc firstItem[T](os: OrderedSet[T]): T =
  for i in os:
    return i

proc getItemIfSingle[T](s: HashSet[T]): T =
  nimyaccAssert s.card == 1, "Expected singleton set but got " & $s
  if s.card == 1:
    for i in s:
      return i

## Same as Dragonbook Argorithm 4.62 & 4.63
proc toLALRKernel(lrKernel: SetOfLRItems, g: Grammar,
                     tt: TransTable): SetOfLALRItems =
  # init result
  nimyaccAssert lrKernel.card > 0
  for idx in 0..<lrKernel.card:
    result.incl(initLALRItems())
  var
    propagation: PropagateTable = initPropagateTable()
    checkSet: HashSet[LALRItem] = initLALRItems()

  # only starting rule
  let startingRule = lrKernel.firstItem.getItemIfSingle
  result[0].incl(startingRule.toLALRItem(End()))
  checkSet.incl(startingRule.toLALRItem(End()))

  # init collection and cal propagate
  for idx, itms in lrKernel:
    for itm in itms:
      if not (propagation.haskey(itm)):
        propagation[itm] = initHashSet[(int, LRItem)]()

      # Dummy is "#" in dragonbook
      let clsr = g.closure(itm.toLALRItem(Dummy()))
      for ci in clsr:
        if ci.ahead == Dummy():
          if ci.next != End():
            propagation[itm].incl (tt[idx][ci.next], ci.forward.toLRItem)
        else:
          let prpgtd = ci.forward
          nimyaccAssert tt[idx][ci.next] < lrKernel.card
          result[tt[idx][ci.next]].incl(prpgtd)
          checkSet.incl(prpgtd)

  # cal collection
  while checkSet.card > 0:
    var newSet = initLALRItems()
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

proc makeTableLALR*(g: Grammar, dctx: var DebugContext): ParsingTable =
  var
    actionTable: ActionTable
    gotoTable: GotoTable
  actionTable = initTable[State, ActionRow]()
  gotoTable = initTable[State, GotoRow]()
  let
    ag = if g.isAugment:
           g
         else:
           g.augment
  let 
    (cc, tt) = makeCanonicalCollection(ag)
    knl = cc.filterKernel
  let lalrKnl = knl.toLALRKernel(ag, tt)
  var cntSR = 0 
  var cntRR = 0 
  for idx, itms in lalrKnl:
    actionTable[idx] = initTable[Symbol, ActionTableItem]()
    gotoTable[idx] = initTable[Symbol, State]()
    let clsr = ag.closure(itms)
    var cnt = 1
    for itm in clsr:
      inc(cnt)
      let sym = itm.nextSkipEmpty
      match sym:
        ErrorS:
          actionTable[idx][sym] = Shift(tt[idx][sym])
        TermS:
          if actionTable[idx].haskey(sym) and
              actionTable[idx][sym].kind == ActionTableItemKind.Reduce:
            actionTable[idx][sym] = resolveShiftReduceConflict(actionTable[idx][sym].rule, sym.term, g, tt[idx][sym])
            if dctx.doGenDebugString:
              dctx.debugStr.add "LALR:Shift-Reduce CONFLICT!!!", idx, ":", sym 
              dctx.debugStr.add "Resolved in favor of ", actionTable[idx][sym]
            inc cntSR 
          elif actionTable[idx].haskey(sym) and
              actionTable[idx][sym].kind == ActionTableItemKind.Error:
            continue 
          else:
            actionTable[idx][sym] = Shift(tt[idx][sym])
        NonTermS:
          gotoTable[idx][sym] = tt[idx][sym]
        End:
          if itm.rule.left == ag.start:
            actionTable[idx][End()] = Accept()
          else:
            if actionTable[idx].haskey(itm.ahead) and
               actionTable[idx][itm.ahead].kind == ActionTableItemKind.Shift:

              # just prioritize shifting error symbol. 
              if itm.ahead.kind == SymbolKind.ErrorS:
                continue 

              actionTable[idx][itm.ahead] = resolveShiftReduceConflict(itm.rule, 
              itm.ahead.term, g, actionTable[idx][itm.ahead].state)
              if dctx.doGenDebugString:
                dctx.debugStr.add "LALR:Shift-Reduce CONFLICT!!!", idx, ":", itm.ahead
                dctx.debugStr.add "Conflict resolved in favor of ", actionTable[idx][itm.ahead]
              inc cntSR 
            elif actionTable[idx].haskey(itm.ahead) and
               actionTable[idx][itm.ahead].kind == ActionTableItemKind.Reduce: 
              if dctx.doGenDebugString:
                dctx.debugStr.add "LALR:Reduce-Reduce CONFLICT!!!", idx, ":", itm.ahead, ".  This usually indicates a serious error in the grammar. It could also be due to the LALR table compression, where multiple reducible rules are placed into the same parser state and there is insufficient context to distinguish them. A possible solution is to add a bogus token to one of the rules to force it into a distinct parser state. Another possible solution is to rewrite the grammar rules to reduce ambiguity." 
              inc cntRR
              continue
            elif actionTable[idx].haskey(itm.ahead) and
               actionTable[idx][itm.ahead].kind == ActionTableItemKind.Error:
              continue
            else:
              actionTable[idx][itm.ahead] = Reduce(itm.rule)
        _:
          discard

  if dctx.doGenDebugString:
    dctx.debugStr.add $g
    dctx.debugStr.add cntSR, " shift-reduce conflict(s)"
    dctx.debugStr.add cntRR, " reduce-reduce conflict(s)"
    dctx.debugStr.add "=============================="
    dctx.debugStr.add "LALR automaton\n"
    for idx, itms in lalrKnl:
      dctx.debugStr.add "state ", idx 
      var clj = ag.closure(itms)
      dctx.debugStr.add lalrItemsToString(clj, g, actionTable, gotoTable, idx)
      dctx.debugStr.add "=============================="
    dctx.debugStr.add actionTable 
    dctx.debugStr.add gotoTable

  if dctx.doGenGraphViz:
    var dg = emptyDotGraph()
    # repeat the closure calculation that we done above. 
    # oh well. 
    for idx, itms in lalrKnl:
      var clj = ag.closure(itms)
      populateDotGraph(dg, clj, g, actionTable, gotoTable, idx)
    dctx.dotStr.add dg.render    

  result = ParsingTable(action: actionTable, goto: gotoTable)