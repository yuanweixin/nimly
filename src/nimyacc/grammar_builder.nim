import parsetypes, sets, tables, options, sequtils

const startRuleNonTerm = "__Start__"

type GrammarBuilder* = object
    rules: OrderedSet[Rule]
    start: Symbol
    precAssoc: Table[int,(Precedence, Associativity)]

func newGrammarBuilder*(start: Symbol) : GrammarBuilder = 
    result.start = start 

func validateLeft(left: Symbol) = 
    doAssert left.kind == SymbolKind.NonTermS, "Left side of rule must be Non-Terminal Symbol."
    doAssert left.nonTerm != startRuleNonTerm, "__Start__ is reserved and cannot be used as lhs of a grammar rule"

func validateRight(r: Symbol) = 
    doAssert r.kind in {SymbolKind.TermS, SymbolKind.ErrorS, SymbolKind.Empty, SymbolKind.NonTermS}

func validateRight(rs: varargs[Symbol]) = 
    for r in rs:
        validateRight(r)

func addRule*(gb: var GrammarBuilder,  r: Rule) = 
    doAssert r notin gb.rules, "Duplicate rule in grammar: " & $r
    gb.rules.incl r

func addRule*(gb: var GrammarBuilder,  left: Symbol, right: Symbol) = 
    let r = Rule(left: left, right: @[right], prec: none[Precedence](), index: 0)
    gb.addRule r

func addRule*(gb: var GrammarBuilder,  left: Symbol, right: varargs[Symbol]) = 
    let r = Rule(left: left, right: right.toSeq, prec: none[Precedence](), index: 0)
    gb.addRule r 

func addRule*(gb: var GrammarBuilder,  prec: Option[Precedence], left: Symbol, right: Symbol) = 
    let r = Rule(left: left, right: @[right], prec: prec, index: 0)
    gb.addRule r

func addRule*(gb: var GrammarBuilder,  prec: Option[Precedence], left: Symbol, right: varargs[Symbol]) = 
    let r = Rule(left: left, right: right.toSeq, prec: prec, index: 0)
    gb.addRule r 
  
func addPrecAssoc*(gb: var GrammarBuilder, tok: int, prec: int, assoc: Associativity) = 
    doAssert tok notin gb.precAssoc, "Trying to add (precedence, associativity) = " & $prec & "," & $assoc & " but it has been added already. Check your specification"
    gb.precAssoc[tok] = (prec, assoc)
  
func toGrammar*(gb: var GrammarBuilder) : Grammar = 
    let
        start = NonTermS(startRuleNonTerm)
        startRule = Rule(prec:none[Precedence](), left: start, right: @[gb.start], index: 0)
    var curIdx = 1 
    var rules = @[startRule]
    for r in gb.rules:
        validateLeft(r.left)
        validateRight(r.right)
        rules.add Rule(left: r.left, right: r.right, prec: r.prec, index: curIdx)
        inc curIdx
    result = Grammar(rules: rules, start: start, precAssoc: gb.precAssoc)
    result.firstTable = result.makeFirstTable
    result.followTable = result.makeFollowTable