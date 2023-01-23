import parsetypes
import dotted
import options
import tables
import sets

const
  nimydebug* {.strdefine.} = ""
  nimygraphviz* {.strdefine.} = ""


type RuleIdx = int 

const errorState : State = -1 
const acceptState : State = -2
const reduceNodeColor = "1"
const acceptNodeColor = "3"
const errorNodeColor = "5"

type DebugContext* = object
  doGenDebugString* : bool
  doGenGraphViz* : bool  
  dotStr* : string 
  debugStr* : string 

proc add*(s: var string, itms: varargs[string, `$`]) = 
  ## this terminates with a newline. 
  for i in itms:
    s.add i
  s.add "\n"

proc `$`*(s: SetOfLRItems): string =
  result = "CanonicalCollection:\n--------\n"
  for i, itms in s:
    result = result & $i & ":" & $itms & "\n"
  result = result & "--------\n"

func findRuleIdx(g: Grammar, r: Rule) : State = 
  #TODO deprecate me and use the index already embedded
  for i,ru in g.rules:
    if r==ru:
      return i 
  return -1 

func populateRuleStringWithDot(result: var string, lhs: string, rhs: seq[Symbol], position:int) = 
  result.add lhs
  result.add " -> "
  for pos, r in rhs:
    if pos == position:
      result.add ". "
    result.add $r
    result.add " "
  if position == rhs.len:
    result.add ". "

proc `$`*(i: LRItem) : string = 
  populateRuleStringWithDot(result, i.rule.left.nonTerm, i.rule.right, i.pos)

proc `$`*(i: LALRItem) : string = 
  populateRuleStringWithDot(result, i.rule.left.nonTerm, i.rule.right, i.pos)
  result.add " ["
  result.add $i.ahead
  result.add "]"

proc `$`*(i: LALRItem, lookaheads: HashSet[Symbol]=initHashSet[Symbol]()): string = 
  populateRuleStringWithDot(result, i.rule.left.nonTerm, i.rule.right, i.pos)
  result.add " ["
  var i = 0 
  for la in lookaheads:
      result.add $la
      if i < lookaheads.len-1:
        result.add ", "
      inc i 
  result.add "]"

func lalrItemGroupTokensToString(result: var string, g: Grammar, itms: LALRItems, separator:string="\n") = 
  var rulePosToLookaheads : Table[(RuleIdx,int), HashSet[Symbol]]
  for i in itms:
    let ruleIdx = findRuleIdx(g, i.rule)
    if (ruleIdx, i.pos) notin rulePosToLookaheads:
      rulePosToLookaheads[(ruleIdx,i.pos)] = [i.ahead].toHashSet
    else:
      rulePosToLookaheads[(ruleIdx,i.pos)].incl i.ahead 
  for i in itms:
    let ruleIdx = findRuleIdx(g, i.rule)
    if (ruleIdx,i.pos) notin rulePosToLookaheads:
      continue 
    result.add $ruleIdx
    result.add " "
    result.add `$`(i, rulePosToLookaheads[(ruleIdx,i.pos)])
    result.add separator
    rulePosToLookaheads.del((ruleIdx,i.pos)) 

func populateActionString*(result: var string, at: var ActionTable, gt: var GotoTable, state: State) = 
  if state in gt:
    for sym, gotoState in gt[state]:
      result.add $sym
      result.add ", go to state "
      result.add $gotoState
      result.add "\n"
  if state in at:
    for sym, act in at[state]:
      case act.kind
      of ActionTableItemKind.Shift:
        result.add $sym
        if sym.kind == SymbolKind.ErrorS: 
          # sym is the `error` token that can be specified in the grammar
          # here, it means, should we shift the error token, we will go 
          # to that state. 
          result.add ", shift error and go to state "
        else:
          result.add ", shift and go to state "
        result.add $act.state
      of ActionTableItemKind.Reduce:
        # unlike for dot graph, we do not need to dedup the reduce here, 
        # because we also print out which symbol was used as the lookahead
        # for the reduce, so there is in fact no duplicated entries, albeit 
        # we do print one line for each (rule,position,lookahead) combination.
        result.add $sym
        result.add ", reduce using "
        result.add $act.rule
      of ActionTableItemKind.Error:
        # here we have an explicit table entry for Error. normally, the absence 
        # of action causes the parser engine to treat it as Error. however, for 
        # non-associative tokens, during shift-reduce conflict resolution, we 
        # would generate an Error entry. 
        result.add $sym
        result.add ", result in parse error"
      of ActionTableItemKind.Accept:
        result.add $sym
        result.add ", accept"
      result.add "\n"

proc lalrItemsToString*(itms: LALRItems, g: Grammar, at: var ActionTable, gt: var GotoTable, state: State) : string = 
  lalrItemGroupTokensToString(result, g, itms)
  result.add "\nactions:\n"
  populateActionString(result, at, gt, state)

proc lrItemsToString*(itms: LRItems, g: Grammar, at: var ActionTable, gt: var GotoTable, state: State) : string = 
  for i in itms:
    let ruleIdx = g.findRuleIdx(i.rule)
    result.add $ruleIdx
    result.add " "
    populateRuleStringWithDot(result, i.rule.left.nonTerm, i.rule.right, i.pos)
    result.add "\n"
  result.add "\nactions:\n"
  populateActionString(result, at, gt, state)

func quote(s: string) : string = 
  result = "\"" & s & "\""

proc emptyDotGraph*() : Graph = 
  result = newGraph(isDirected=true)
  result.nodeAttrs.add ("fontname", "courier")
  result.nodeAttrs.add ("shape", "box")
  result.nodeAttrs.add ("colorscheme", "paired6")
  result.edgeAttrs.add ("fontname", "courier")
  discard result.node($acceptState, label=some "accept", [("fillcolor", acceptNodeColor), ("shape","square"), ("style", "filled")])
    .node($errorState, label=some "parse error", [("fillcolor", errorNodeColor), ("shape", "diamond"), ("style", "filled")])

func populateGraphNodeAction(gr: var Graph, g: Grammar, at: var ActionTable, gt: var GotoTable, state: State) = 
  var reduceDedup: HashSet[State]
  if state in gt:
    for sym, gotoState in gt[state]:
      discard gr.edge($state, $gotoState, some $sym, [("style", "dashed")]) # e.g. 1 --left--> 2
  if state in at:
    for sym, act in at[state]:
      case act.kind
      of ActionTableItemKind.Shift:
          # sym is the `error` token that can be specified in the grammar
          # here, it means, should we shift the error token, we will go 
          # to that state. 
          if sym.kind == SymbolKind.ErrorS:
            discard gr.edge($state, $act.state, none[string](), [("style", "dotted")]) # no label on error 
          else:
            discard gr.edge($state, $act.state, some $sym, [("style", "solid")])
      of ActionTableItemKind.Reduce:
        let ruleIdx = g.findRuleIdx(act.rule)
        if ruleIdx notin reduceDedup:
            # e.g. 1R5
          let reduceStateName = $state & "R" & $ruleIdx
          # need to quote 1R5 
          discard gr.node(quote reduceStateName, label=some("R" & $ruleIdx), [("fillcolor", reduceNodeColor), ("shape", "diamond"), ("style", "filled")]) 
            .edge($state, quote reduceStateName, none[string](), [("style", "solid")]) 
          reduceDedup.incl ruleIdx
      of ActionTableItemKind.Error:
        # edge to the error node because, well, at run time this depends on the 
        # left parse context, so we wouldn't really know after we pop off some left
        # context what state we end up being able to Shift to with the error 
        discard gr.edge($state, $errorState, some $sym, [("style", "dotted")])
      of ActionTableItemKind.Accept:
        discard gr.edge($state, $acceptState, some $sym, [("style", "solid")])
  
proc populateDotGraph*(gr: var Graph, itms: LRItems, g: Grammar, at: var ActionTable, gt: var GotoTable, state: State) = 
  var ns = "State " & $state & "\\n\\l "
  for i in itms:
    let ruleIdx = g.findRuleIdx(i.rule)
    ns.add $ruleIdx
    ns.add " "
    populateRuleStringWithDot(ns, i.rule.left.nonTerm, i.rule.right, i.pos)
    ns.add "\\n\\l "
  discard gr.node($state, some ns) # add the state node
  populateGraphNodeAction(gr, g, at, gt, state)

proc populateDotGraph*(gr: var Graph, itms: LALRItems, g: Grammar, at: var ActionTable, gt: var GotoTable, state: State) = 
  var ns = "State " & $state & "\\n\\l "
  lalrItemGroupTokensToString(ns, g, itms, separator="\\n\\l")
  discard gr.node($state, some ns) # add the state node
  populateGraphNodeAction(gr, g, at, gt, state)
      
proc `$`*(r: Symbol) : string = 
  case r.kind
    of SymbolKind.TermS:
      result.add $r.term
    of SymbolKind.NonTermS:
      result.add $r.nonTerm
    of SymbolKind.Dummy:
      result.add "#"
    of SymbolKind.End:
      result.add "$"
    of SymbolKind.Empty:
      result.add "epsilon"
    of SymbolKind.ErrorS:
      result.add "error"

proc `$`*(rule: Rule) : string = 
  result.add rule.left.nonTerm
  result.add " -> "
  for i,r in rule.right:
    result.add $r
    if i < rule.right.len-1:
      result.add " " 

proc `$`*(ft: FollowTable): string =
  result = "\n--------\n"
  for i, itms in ft:
    result = result & $i & ":" & $itms & "\n"
  result = result & "--------\n"

proc `$`*(g: Grammar): string = 
    result = "\n---- Grammar ----\n"
    for i,r in g.rules:
      result.add $i
      result.add " " 
      result.add $r
      result.add "\n"
    
proc `$`*(at: ActionTable): string =
  result = "\nActionTable:\n--------\n"
  for s, row in at:
    result = result & $s & ":" & $row & "\n"
  result = result & "--------\n"

proc `$`*(gt: GotoTable): string =
  result = "\nGotoTable:\n--------\n"
  for s, row in gt:
    result = result & $s & ":" & $row & "\n"
  result = result & "--------\n"
