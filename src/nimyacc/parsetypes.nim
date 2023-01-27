import tables
import sets
import hashes

import patty
import options
import dev_assert
import sequtils

type
  NimyError* = object of CatchableError
  NimyActionError* = object of NimyError
  NimyGotoError* = object of NimyError

  sym* = string
  SymbolKind* {.pure.} = enum
    TermS
    NonTermS
    Dummy # '#' in dragon book for computing propagated lookahead 
    End   # '$' endmarker in dragon book. 
    Empty # epsilon 
    ErrorS # the error token for implementing panic recovery

  Symbol* = object
    case kind*: SymbolKind
    of SymbolKind.TermS:
      term*: int
    of SymbolKind.NonTermS:
      nonTerm*: sym
    else:
      discard

  Rule* = object
    left*: Symbol
    right*: seq[Symbol]
    prec*: Option[Precedence]
    index*: int # auxiliary info, does not participate in == or hash

  Associativity* = enum
    Left
    Right
    NonAssoc

  Precedence* = int 

  Grammar* = ref GrammarObj
  GrammarObj* = object
    rules*: seq[Rule]
    start*: Symbol
    firstTable*: FirstTable
    followTable*: FollowTable
    precAssoc*: Table[int,(Precedence, Associativity)]

  FollowTable* = Table[Symbol, HashSet[Symbol]]
  FirstTable* = Table[Symbol, HashSet[Symbol]]

type
  LRItem* = object
    ruleIdx*: int 
    pos*: int
    g*: Grammar # auxiliary, does not participate in == or hash

  LRItems* = HashSet[LRItem] 
  SetOfLRItems* = OrderedSet[LRItems]

  TransTable* = seq[Table[Symbol, int]]

type
  LALRItem* = object
    ruleIdx*: int
    pos*: int
    ahead*: Symbol
    g*: Grammar # auxiliary, does not participate in == or hash
  LALRItems* = HashSet[LALRItem]
  SetOfLALRItems* = OrderedTable[int, LALRItems]
  PropagateTable* = Table[LRItem, HashSet[(int, LRItem)]]

type
  State* = int
  ActionTableItemKind* {.pure.} = enum
    Shift
    Reduce
    Accept
    Error
  ActionTableItem* = object
    case kind*: ActionTableItemKind:
    of ActionTableItemKind.Shift:
      state*: State
    of ActionTableItemKind.Reduce:
      rule*: Rule
    of ActionTableItemKind.Accept:
      startRule*: Rule
    else:
      discard

type
  ActionRow* = Table[Symbol, ActionTableItem]
  ActionTable* = Table[State, ActionRow]
  GotoRow* = Table[Symbol, State]
  GotoTable* = Table[State, GotoRow]
  ParsingTable* = object
    action*: ActionTable
    goto*: GotoTable
  Parser* = object
    stack*: seq[State]
    table*: ParsingTable
    provisionalToksCnt*: int 
    hasError*: bool
    onError*: proc (input:string, startPos: int, endPosExcl: int) {.closure.}
    onEof*: proc (input:string, pos: int) {.closure.}

variantp ParseTree[T]:
  Terminal(token: T, tspos: int, tepos: int) # tspos, tepos = first, last char pos of token
  ErrorNode(errpos: int) ## error token. errpos = pos where the offending token occurs 
  NonTerminal(rule: Rule, ntspos: int, ntepos:int, tree: seq[ParseTree[T]]) ## ntspos, ntepos = leftmost, rightmost pos in this nonterminal. they can be the same if rhs derived empty.

func rule*(i: LRItem) : Rule = 
  return i.g.rules[i.ruleIdx]

func rule*(i:LALRItem) : Rule = 
  return i.g.rules[i.ruleIdx]

proc len*(r: Rule): int =
  return r.right.len

proc `==`*(a, b: Symbol): bool =
  if a.kind != b.kind:
    return false
  match a:
    TermS(term: t):
      return t == b.term
    NonTermS(nonTerm: nt):
      return nt == b.nonTerm
    _:
      return true

proc `==`*(a,b: Rule): bool = 
  return a.left == b.left and a.right == b.right and a.prec == b.prec   

proc `==`*(a,b: LRItem): bool = 
  return a.ruleIdx == b.ruleIdx and a.pos == b.pos

proc `==`*(a,b: LALRItem): bool = 
  return a.ruleIdx == b.ruleIdx and a.pos == b.pos and a.ahead == b.ahead

# =============================== ctors =======================================
proc Shift*(state: State): ActionTableItem =
  return ActionTableItem(kind: ActionTableItemKind.Shift, state: state)

proc Reduce*(rule: Rule): ActionTableItem =
  return ActionTableItem(kind: ActionTableItemKind.Reduce, rule: rule)

proc Accept*(rule: Rule): ActionTableItem =
  return ActionTableItem(kind: ActionTableItemKind.Accept, startRule: rule)

proc Error*(): ActionTableItem =
  return ActionTableItem(kind: ActionTableItemKind.Error)

proc NonTermS*(nonTerm: sym): Symbol =
  return Symbol(kind: SymbolKind.NonTermS, nonTerm: nonTerm)

proc Dummy*(): Symbol =
  return Symbol(kind: SymbolKind.Dummy)

proc End*(): Symbol =
  return Symbol(kind: SymbolKind.End)

proc Empty*(): Symbol =
  return Symbol(kind: SymbolKind.Empty)

proc TermS*(term: int): Symbol =
  return Symbol(kind: SymbolKind.TermS, term: term)

proc ErrorS*() : Symbol = 
  return Symbol(kind: SymbolKind.ErrorS)

proc hash*(x: Symbol): Hash =
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

proc hash*(x: Rule): Hash =
  var h: Hash = 0
  h = h !& hash(x.left)
  h = h !& hash(x.right)
  h = h !& hash(x.prec)
  return !$h

proc hash*(x: LRItem): Hash =
  var h: Hash = 0
  h = h !& hash(x.ruleIdx)
  h = h !& hash(x.pos)
  return !$h

proc hash*(x: LALRItem): Hash =
  var h: Hash = 0
  h = h !& hash(x.ruleIdx)
  h = h !& hash(x.pos)
  h = h !& hash(x.ahead)
  return !$h

proc lenWithoutEmpty*(r: Rule): int =
  result = 0
  for s in r.right:
    if s != Empty():
      inc(result)

proc newRule*(prec: Option[Precedence], left: Symbol, right: varargs[Symbol]): Rule =
  result = Rule(left: left, right: right.toSeq, prec: prec)

proc newRule*(prec: Option[Precedence], left: Symbol, right: Symbol): Rule =
  result = Rule(left: left, right: @[right], prec: prec)

proc initGrammar*(rules: seq[Rule], start: Symbol): Grammar =
  result = Grammar(rules: rules, start: start)

proc initGrammar*(rules: openArray[Rule],
                     start: Symbol): Grammar =
  result = initGrammar(rules.toSeq, start)

iterator filterRulesLeftIs*(g: Grammar, x: Symbol): Rule = 
  for r in g.rules:
    if r.left == x:
      yield r 

proc isAugment*(g: Grammar): bool =
  result = (g.start == NonTermS("__Start__"))

proc startRule*(g: Grammar): Rule =
  nimyaccAssert g.isAugment, "`g` is not augmented grammar."
  var cnt = 0 
  for r in g.filterRulesLeftIs(g.start):
    result = r 
    inc cnt 
  nimyaccAssert (cnt == 1), "`g` is invalid grammar. Found more than 1 start rule."

proc symbolSet*(g: Grammar): HashSet[Symbol] =
  for r in g.rules: 
    for s in r.right:
      result.incl(s)
  result.incl(g.start)

proc nonTermSymbolSet*(g: Grammar): HashSet[Symbol] =
  for r in g.rules:
    for s in r.right: 
      if s.kind == SymbolKind.NonTermS:
        result.incl(s)
  result.incl(g.start)

proc containsOrIncl*(s: var HashSet, other: HashSet): bool =
  result = true
  for item in other:
    result = result and containsOrIncl(s, item)

proc makeFirstTable*(g: Grammar): FirstTable =
  result = initTable[Symbol, HashSet[Symbol]]()
  for s in g.symbolSet:
    match s:
      NonTermS:
        result[s] = initHashSet[Symbol]()
      TermS:
        result[s] = [s].toHashSet
      Empty: # shows up if Empty is on the rhs of some X -> Y1...Yk 
        result[s] = [s].toHashSet
      ErrorS:
        # error symbol is treated as a terminal for parser generation
        result[s] = [s].toHashset 
      _:
        doAssert false, "There is a non-symbol in rules."

  # X -> epsilon, include empty for FIRST(X)
  for r in g.rules:
    if r.right.len == 0:
      result[r.left].incl(Empty())

  var fCnt = true
  while fCnt:
    fCnt = false
    # convoluted loop to go through rhs left to right for each production. 
    for r in g.rules:
      var fEmp = true
      for s in r.right:
        # add any terminals we have for this. 
        let newFst = result[r.left] + (result[s] - [Empty()].toHashSet)
        if result[r.left] != newFst:
          fCnt = true
        result[r.left] = newFst
        if not result[s].contains(Empty()): # Yi is not nullable. 
          fEmp = false
          break
      if fEmp: # X -> Y1...Yk and all Y1...Yk are nullable. 
        if not result[r.left].containsOrIncl(Empty()):
          fCnt = true

proc makeFollowTable*(g: Grammar): FollowTable =
  nimyaccAssert g.firstTable.len != 0, "firstTable is nil."
  result = initTable[Symbol, HashSet[Symbol]]()
  for s in g.nonTermSymbolSet:
    result[s] = initHashSet[Symbol]()
  # place $ in FOLLOW(S). 
  result[g.start].incl(End())
  var fCnt = true
  while fCnt:
    fCnt = false
    for r in g.rules:
      var
        fEmpTail = true
        # firstSyms is the first tokens of the last processed rhs symbol 
        # (we process rhs symbols right to left)
        firstSyms: HashSet[Symbol] 
      
      for i in countdown(r.right.len - 1, 0):
        let sym = r.right[i]
        # A -> aBb, everything in FIRST(b)-{Empty} is in FOLLOW(B)
        match sym:
          TermS:
            # renew meta data
            fEmpTail = false
            firstSyms = [sym].toHashSet
          ErrorS:
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
            if fsts.contains(Empty()):
              for f in fsts:
                firstSyms.incl(f)
            else:
              fEmpTail = false
              firstSyms = fsts
          _:
            doAssert false, "This is not a valid symbol in a rule: " & $sym

proc augment*(g: Grammar): Grammar =
  let
    start = NonTermS("__Start__")
    startRule = newRule(prec=none[Precedence](), left = start, right = g.start)
  if g.rules.contains(startRule):
    result = initGrammar(g.rules, start)
  else:
    var newRules : seq[Rule] 
    newRules.add startRule
    for r in g.rules:
      newRules.add r
    result = initGrammar(newRules, start)
  result.firstTable = result.makeFirstTable
  result.followTable = result.makeFollowTable

iterator fromNextNext(i: LALRItem): Symbol = 
  nimyaccAssert i.pos < i.rule.len
  for index in (i.pos + 1)..<i.rule.len:
    yield i.rule.right[index]

proc calFirsts*(g: Grammar,
                   itm: LALRItem) : HashSet[Symbol] = 
  ## assumption: grammar is augmented.
  for s in itm.fromNextNext():
    match s:
      NonTermS:
        let firsts = g.firstTable[s]
        result.incl(firsts)
        if not (Empty() in firsts):
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
      ErrorS:
        result.incl(s)
        return 
      Empty:
        raise newException(
          NimyError,
          "Unexpected Empty Rule: Ambiguous rules for parse. Epsilon cannot be used on the right hand side of rules."
        )
  result.incl itm.ahead

proc getPrecedence*(g: Grammar, tok: int): Option[Precedence] = 
  if tok in g.precAssoc:
    let (prec,_) = g.precAssoc[tok]
    return some(prec)
  return none[Precedence]()

proc getAssociativity*(g: Grammar, tok: int): Option[Associativity] = 
  if tok in g.precAssoc:
    let (_,assoc) = g.precAssoc[tok]
    return some(assoc)
  return none[Associativity]()

proc getPrecedence*(g: Grammar, r: Rule) : Option[Precedence] = 
  if r.prec.isSome:
    return r.prec
  for i in countdown(r.len-1, 0):
    case r.right[i].kind
    of SymbolKind.TermS:
      if r.right[i].term in g.precAssoc:
        let (prec, _) = g.precAssoc[r.right[i].term]
        return some(prec)
      return none[Precedence]()
    else:
      continue 
  return none[Precedence]()

proc next*(i: LRItem): Symbol =
  ## symbol to the right of the dot 
  if i.pos >= i.rule.len:
    return End()
  result = i.rule.right[i.pos]

proc nextSkipEmpty*(i: LRItem): Symbol =
  result = End()
  for idx in i.pos..<i.rule.len:
    let nxt = i.rule.right[idx]
    if nxt != Empty():
      result = nxt
      break

proc next*(i: LALRItem): Symbol =
  if i.pos >= i.rule.len:
    return End()
  result = i.rule.right[i.pos]

proc nextSkipEmpty*(i: LALRItem): Symbol =
  result = End()
  for idx in i.pos..<i.rule.len:
    let nxt = i.rule.right[idx]
    if nxt != Empty():
      result = nxt
      break

proc toLRItem*(lalrItem: LALRItem): LRItem =
  result = LRItem(ruleIdx: lalrItem.ruleIdx, pos: lalrItem.pos, g: lalrItem.g)

