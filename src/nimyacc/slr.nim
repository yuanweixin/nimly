import tables
import sets
import patty
import parsetypes
import options
import dev_assert

proc initTransTableRow(): Table[Symbol, int] =
  result = initTable[Symbol, int]()

proc indexOf[T](os: OrderedSet[T], element: T): int =
  for i, key in os:
    if key == element:
      return i
  return -1

proc pointForward(i: LRItem): LRItem =
  ## move dot up by 1 
  nimyaccAssert i.pos < i.rule.len
  result = LRItem(ruleIdx: i.rule.index, pos: i.pos + 1, g: i.g)

proc closure(g: Grammar, whole: LRItems): LRItems =
  result = whole
  var checkSet = whole
  var new: LRItems
  while checkSet.len > 0:
    for i in checkSet:
      match i.next:
        # S -> p.Nq where N is NonTerm 
        NonTermS:
          # look for N -> ...
          for r in g.filterRulesLeftIs(i.next):
            let n = LRItem(ruleIdx: r.index, pos: 0, g: g)
            if not result.containsOrIncl(n):
              new.incl(n)
        _:
          discard
    checkSet = new
    new.clear()

proc goto(g: Grammar, itms: LRItems, s: Symbol): LRItems =
  nimyaccAssert s.kind != SymbolKind.End
  nimyaccAssert itms == g.closure(itms)
  var gotoHashSet = initHashSet[LRItem]()
  for i in itms:
    if i.next == s:
      gotoHashSet.incl(i.pointForward)
  result = g.closure(gotoHashSet)


proc resolveShiftReduceConflict*(r: Rule, t: int, g: Grammar, state: State): ActionTableItem = 
  let rp = g.getPrecedence(r)
  let tp = g.getPrecedence(t)
  if rp.isSome and tp.isSome:
    if tp.get > rp.get:
      return Shift(state)
    elif rp.get > tp.get:
      return Reduce(r)
    else:
      let assoc = g.getAssociativity(t)
      nimyaccAssert assoc.isSome, "Bug in implementation: rule's precedence token must exist in grammar declaration's associativity declarations, and since it has same precedence as the lookahead, the lookahead must belong to the same line of associativity declaration. But no associativity exists."
      case assoc.get 
      of Left:
        return Reduce(r)
      of Right:
        return Shift(state)
      of NonAssoc:
        return Error()
  return Shift(state)

proc makeCanonicalCollection*(g: Grammar): (SetOfLRItems,
                                                  TransTable) =
  # figure 4.33 of dragon book.                                          
  # seed C with the closure of start rule
  let init = g.closure([LRItem(ruleIdx: g.startRule.index, pos: 0, g: g)].toHashSet)
  var
    cc = [
      init
    ].toOrderedSet
    checkSet = cc
    tt: TransTable = @[]
  tt.add(initTransTableRow())
  when defined(nimydevel):
    var gotoCallSavedCnt = 0
    var crossStateDupGotoCalls = 0
  while checkSet.len > 0:
    var new: SetOfLRItems
    # for each set of items I in C 
    for itms in checkSet:
      let frm = cc.indexOf(itms) 
      nimyaccAssert itms == g.closure(itms)
      var symbolsSeen : HashSet[Symbol] # tracks which symbols we already done transitions for in this automaton state! 
      for i in itms:
        # in textbook algorithm, this is supposed to loop over each grammar symbol. 
        # here we use the (possibly non-existent) symbol to the right of the dot of 
        # every lritem in the automaton state. but, this introduces duplicate calls 
        # to goto when there are more than 1 item that can transition on the same 
        # grammar symbol. there is another source of duplication: different automaton 
        # states can have the same subset of (rule,pos) pairs, e.g. the rules V -> .x,
        # V -> .y can show up in different automaton states. 
        # 
        # currently, we will dedup the ones that arise from the same automaton state. 
        # it's heavier to keep track of sets of lritems so we don't do the dedup across
        # automaton states, and the overall cost of this call isn't too large. 
        let s = i.next
        when defined(nimydevel):
          if s in symbolsSeen:
            inc gotoCallSavedCnt
        if s != End() and s notin symbolsSeen:
          symbolsSeen.incl s
          let gt = goto(g, itms, s) 
          # if GOTO(I,X) exist (it must since we use i.next) and is not in C 
          if (not cc.containsOrIncl(gt)):
            # Add GOTO(I,X) to C 
            tt.add(initTransTableRow())
            nimyaccAssert cc.card == tt.len
            new.incl(gt)
          else:
            when defined(nimydevel):
              echo "goto from ", frm, " to ", s, " already present."
              inc crossStateDupGotoCalls
            discard
          tt[frm][s] = cc.indexOf(gt)
    checkSet = new
  when defined(nimydevel):
    echo "goto calls saved: ", gotoCallSavedCnt
    echo "cross state duplicated goto calls: ", crossStateDupGotoCalls
  nimyaccAssert cc.indexOf(init) == 0, "init state is not '0'"
  result = (cc, tt)

proc filterKernel*(cc: SetOfLRItems): SetOfLRItems =
  result = initOrderedSet[LRItems]()
  let start = NonTermS("__Start__")
  for i, itms in cc:
    for itm in itms:
      var kernelItems = initHashSet[LRItem]()
      for itm in itms:
        if itm.pos != 0 or itm.rule.left == start:
          kernelItems.incl(itm)
      result.incl(kernelItems)
