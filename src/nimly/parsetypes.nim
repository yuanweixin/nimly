import tables
import sets
import hashes

import patty

type
  NimyError* = object of Exception
  NimyActionError* = object of NimyError
  NimyGotoError* = object of NimyError

  sym* = string
  SymbolKind* {.pure.} = enum
    TermS
    NonTermS
    Dummy # '#' in dragon book for computing propagated lookahead 
    End   # '$' endmarker in dragon book. 
    Empty # epsilon 
  Symbol*[T] = object
    case kind*: SymbolKind
    of SymbolKind.TermS:
      term*: T
    of SymbolKind.NonTermS:
      nonTerm*: sym
    else:
      discard

  Rule*[T] = object
    left*: Symbol[T]
    right*: seq[Symbol[T]]

  Grammar*[T] = object
    rules*: HashSet[Rule[T]]
    start*: Symbol[T]
    firstTable*: FirstTable[T]
    followTable*: FollowTable[T]

  FollowTable[T] = Table[Symbol[T], HashSet[Symbol[T]]]
  FirstTable[T] = Table[Symbol[T], HashSet[Symbol[T]]]

proc len*[T](r: Rule[T]): int =
  return r.right.len

proc `==`*[T](a, b: Symbol[T]): bool =
  if a.kind != b.kind:
    return false
  match a:
    TermS(term: t):
      return t == b.term
    NonTermS(nonTerm: nt):
      return nt == b.nonTerm
    _:
      return true

# =============================== ctors =======================================
proc NonTermS*[T](nonTerm: sym): Symbol[T] =
  return Symbol[T](kind: SymbolKind.NonTermS, nonTerm: nonTerm)

proc Dummy*[T](): Symbol[T] =
  return Symbol[T](kind: SymbolKind.Dummy)

proc End*[T](): Symbol[T] =
  return Symbol[T](kind: SymbolKind.End)

proc Empty*[T](): Symbol[T] =
  return Symbol[T](kind: SymbolKind.Empty)

proc TermS*[T](term: T): Symbol[T] =
  return Symbol[T](kind: SymbolKind.TermS, term: term)

proc `$`*[T](ft: FollowTable[T]): string =
  result = "FollowTable:\n--------\n"
  for i, itms in ft:
    result = result & $i & ":" & $itms & "\n"
  result = result & "--------\n"

proc hash*[T](x: Symbol[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.kind)
  match x:
    TermS(term: s):
      h = h !& hash(s)
    NonTermS(nonTerm: s):
      h = h !& hash(s)
    _:
      discard
  return !$h

proc hash*[T](x: Rule[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.left)
  h = h !& hash(x.right)
  return !$h

proc lenWithoutEmpty*[T](r: Rule[T]): int =
  result = 0
  for s in r.right:
    if s != Empty[T]():
      inc(result)

proc `[]`[T](os: OrderedSet[T], idx: int): T {.inline.} =
  if os.len <= idx:
    raise newException(IndexDefect, "idx is too large.")
  for i, key in os:
    if i == idx:
      return key

proc newRule*[T](left: Symbol[T], right: varargs[Symbol[T]]): Rule[T] =
  assert left.kind == SymbolKind.NonTermS,
     "Left side of rule must be Non-Terminal Symbol."
  var rightSeq: seq[Symbol[T]] = @[]
  for s in right:
    rightSeq.add(s)

  result = Rule[T](left: left, right: rightSeq)

proc newRule*[T](left: Symbol[T], right: Symbol[T]): Rule[T] =
  assert left.kind == SymbolKind.NonTermS,
     "Left side of rule must be Non-Terminal Symbol."
  result = Rule[T](left: left, right: @[right])

proc initGrammar*[T](rules: HashSet[Rule[T]], start: Symbol[T]): Grammar[T] =
  result = Grammar[T](rules: rules, start: start)

proc initGrammar*[T](rules: openArray[Rule[T]],
                     start: Symbol[T]): Grammar[T] =
  result = initGrammar(rules.toHashSet, start)

proc filterRulesLeftIs*[T](g: Grammar[T], x: Symbol[T]): seq[Rule[T]] =
  result = @[]
  for r in g.rules:
    if r.left == x:
      assert (not (r in result)), "x in result."
      result.add(r)

proc isAugment*[T](g: Grammar[T]): bool =
  result = (g.start == NonTermS[T]("__Start__"))
  assert (g.filterRulesLeftIs(g.start).len != 0),
     "`g` is invalid grammar."

proc startRule*[T](g: Grammar[T]): Rule[T] =
  doAssert g.isAugment, "`g` is not augmented grammar."
  let ret = g.filterRulesLeftIs(g.start)
  doAssert (ret.len == 1), "`g` is invalid augment grammar."
  for r in ret:
    result = r

proc symbolSet*[T](g: Grammar[T]): HashSet[Symbol[T]] =
  for r in g.rules: # TODO assumes rhs are reachable from start. Add validation
    for s in r.right:
      result.incl(s)
  result.incl(g.start)

proc nonTermSymbolSet*[T](g: Grammar[T]): HashSet[Symbol[T]] =
  for r in g.rules:
    for s in r.right: # TODO assumes rhs reachable from start. Add validation. 
      if s.kind == SymbolKind.NonTermS:
        result.incl(s)
  result.incl(g.start)

proc containsOrIncl*[T](s: var HashSet[T], other: HashSet[T]): bool =
  result = true
  assert s.isValid, "The set `s` needs to be initialized."
  assert other.isValid, "The set `other` needs to be initialized."
  for item in other:
    result = result and containsOrIncl(s, item)

proc makeFirstTable[T](g: Grammar[T]): FirstTable[T] =
  result = initTable[Symbol[T], HashSet[Symbol[T]]]()
  for s in g.symbolSet:
    match s:
      NonTermS:
        result[s] = initHashSet[Symbol[T]]()
      TermS:
        result[s] = [s].toHashSet
      Empty: # shows up if Empty is on the rhs of some X -> Y1...Yk 
        result[s] = [s].toHashSet
      _:
        doAssert false, "There is a non-symbol in rules."

  # X -> epsilon, include empty for FIRST(X)
  for r in g.rules:
    if r.right.len == 0:
      result[r.left].incl(Empty[T]())

  var fCnt = true
  while fCnt:
    fCnt = false
    # convoluted loop to go through rhs left to right for each production. 
    for r in g.rules:
      var fEmp = true
      for s in r.right:
        # add any terminals we have for this. 
        let newFst = result[r.left] + (result[s] - [Empty[T]()].toHashSet)
        if result[r.left] != newFst:
          fCnt = true
        result[r.left] = newFst
        if not result[s].contains(Empty[T]()): # Yi is not nullable. 
          fEmp = false
          break
      if fEmp: # X -> Y1...Yk and all Y1...Yk are nullable. 
        if not result[r.left].containsOrIncl(Empty[T]()):
          fCnt = true

proc makeFollowTable[T](g: Grammar[T]): FollowTable[T] =
  doAssert g.firstTable.len != 0, "firstTable is nill."
  result = initTable[Symbol[T], HashSet[Symbol[T]]]()
  for s in g.nonTermSymbolSet:
    result[s] = initHashSet[Symbol[T]]()
  # place $ in FOLLOW(S). 
  result[g.start].incl(End[T]())
  var fCnt = true
  while fCnt:
    fCnt = false
    for r in g.rules:
      var
        fEmpTail = true
        # firstSyms is the first tokens of the last processed rhs symbol 
        # (we process rhs symbols right to left)
        firstSyms: HashSet[Symbol[T]] 
      
      for i in countdown(r.right.len - 1, 0):
        let sym = r.right[i]
        assert sym != End[T]()
        # A -> aBb, everything in FIRST(b)-{Empty} is in FOLLOW(B)
        match sym:
          TermS:
            # renew meta data
            fEmpTail = false
            firstSyms = [sym].toHashSet
          Empty:
            discard
          NonTermS:
            # renew first table
            for f in firstSyms:
              let prevFC = fCnt
              fCnt = (not result[sym].containsOrIncl(f))
              fCnt = fCnt or prevFC
            if fEmpTail:
              # A -> aB or A -> aBb with nullable(b), 
              # all of FOLLOW(A) is in FOLLOW(B)
              for f in result[r.left]:
                let prevFC = fCnt
                fCnt = (not result[sym].containsOrincl(f))
                fCnt = fCnt or prevFC

            # renew meta data
            let fsts = g.firstTable[sym]
            # decide if extend or replace firstSym depending on if 
            # the current symbol is nullable. 
            if fsts.contains(Empty[T]()):
              for f in fsts:
                firstSyms.incl(f)
            else:
              fEmpTail = false
              firstSyms = fsts
          _:
            doAssert false, "There is other than Term or NonTerm in Rules."

proc augment*[T](g: Grammar[T]): Grammar[T] =
  let
    start = NonTermS[T]("__Start__")
    startRule = newRule(left = start, right = g.start)
  if g.rules.contains(startRule):
    result = initGrammar(g.rules, start)
  else:
    let newRules = g.rules + [startRule].toHashSet()
    result = initGrammar(newRules, start)
  result.firstTable = result.makeFirstTable
  result.followTable = result.makeFollowTable

proc calFirsts*[T](g: Grammar[T],
                   symbols: seq[Symbol[T]]): HashSet[Symbol[T]] =
  assert g.isAugment
  result.init
  for s in symbols:
    match s:
      NonTermS:
        let firsts = g.firstTable[s]
        result.incl(firsts)
        if not (Empty[T]() in firsts):
          # first non-nullable symbol, end loop. 
          return
      TermS:
        result.incl(s)
        return
      End:
        result.incl(s)
        return
      Dummy:
        result.incl(s)
        return
      Empty:
        raise newException(
          NimyError,
          "Unexpected Empty Rule: Ambiguous rules for parse empty sequent cannot be used in nimy. Check your grammer has no ambiguous rules in which what non-terminal represents the empty sequent."
        )
