import parsetypes
import dotted
import std/options
import std/tables
import std/sets

proc `$`*[T](s: SetOfLRItems[T]): string =
  result = "CanonicalCollection:\n--------\n"
  for i, itms in s:
    result = result & $i & ":" & $itms & "\n"
  result = result & "--------\n"

func populateRuleStringWithDot[T](result: var string, lhs: string, rhs: seq[Symbol[T]], position:int) = 
  result.add lhs
  result.add " -> "
  for pos, r in rhs:
    if pos == position:
      result.add ". "
    result.add $r
    result.add " "
  if position == rhs.len:
    result.add ". "


proc `$`*[T](i: LALRItem[T]) : string = 
  populateRuleStringWithDot(result, i.rule.left.nonTerm, i.rule.right, i.pos)
  result.add " ["
  result.add $i.ahead
  result.add "]"

proc `$`*[T](i: LRItem[T]) : string = 
  populateRuleStringWithDot(result, i.rule.left.nonTerm, i.rule.right, i.pos)

func findRuleIdx[T](g: Grammar[T], r: Rule[T]) : State = 
  for i,ru in g.rules:
    if r==ru:
      return i 
  return -1 

proc lrItemToString[T](r: LRItem[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) : string = 
    let nxt = next(r)
    result.add "\t"
    if nxt == End[T]():
        result.add "reduce using " 
        result.add $r.rule
    elif state in gt and nxt in gt[state]:
        result.add $nxt 
        result.add ", go to state "
        result.add $gt[state][nxt]
    else:
        result.add $nxt 
        if nxt.kind == SymbolKind.ErrorS:
          result.add ", go to state "
        else:
          result.add ", shift and go to state "
        result.add $at[state][nxt].state

proc lalrItemToString[T](r: LALRItem[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) : string = 
    let nxt = next(r)
    result.add "\t"
    if nxt == End[T]():
        result.add $r.ahead
        result.add "\t" 
        result.add "reduce using " 
        result.add $r.rule
    elif state in gt and nxt in gt[state]:
        result.add $nxt 
        result.add ", go to state "
        result.add $gt[state][nxt]
    else:
        result.add $nxt 
        if nxt.kind == SymbolKind.ErrorS:
          result.add ", go to state "
        else:
          result.add ", shift and go to state "
        result.add $at[state][nxt].state

proc lalrItemsToString*[T](itms: LALRItems[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) : string = 
  for r in itms:
    result.add "\t"
    result.add $findRuleIdx(g, r.rule)
    result.add " "
    result.add $r
    result.add "\n"
  result.add "\n"
  for r in itms:
    result.add lalrItemToString(r, g, at, gt, state)
    result.add "\n"

proc lrItemsToString*[T](itms: LRItems[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) : string = 
    for r in itms:
        result.add "\t"
        result.add $findRuleIdx(g, r.rule)
        result.add " "
        result.add $r
        result.add "\n"
    result.add "\n"
    for r in itms:
        result.add lrItemToString(r, g, at, gt, state)
        result.add "\n"

iterator actionTableItems*[T](at: ActionTable[T], state: State) : (Symbol[T], ActionTableItem[T]) = 
  if state in at:
    for sym, item in at[state]:
      yield (sym, item)
      
func quote(s: string) : string = 
  result = "\"" & s & "\""

proc emptyDotGraph*() : Graph = 
  var dg = newGraph(isDirected=true)
  dg.nodeAttrs.add ("fontname", "courier")
  dg.nodeAttrs.add ("shape", "box")
  dg.nodeAttrs.add ("colorscheme", "paired6")
  dg.edgeAttrs.add ("fontname", "courier")
  return dg 

proc populateDotGraph*[T](gr: var Graph, itms: LRItems[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) = 
  var ns = "State " & $state & "\\n\\l "
  # prevent duplicate edges
  var populatedTransitions : HashSet[Symbol[T]]
  for i in itms:
    let ruleIdx = findRuleIdx(g, i.rule)
    ns.add $ruleIdx
    ns.add " "
    populateRuleStringWithDot(ns, i.rule.left.nonTerm, i.rule.right, i.pos)
    ns.add "\\l"
    # remember for which symbols we already drew edge
    let nxt = next(i)
    if nxt in populatedTransitions:
      continue 
    populatedTransitions.incl nxt
    if nxt == End[T]():
        # e.g. 1R5
        let reduceStateName = $state & "R" & $ruleIdx
        # need to quote 1R5 otherwise doesn't work correctly
        discard gr.node(quote reduceStateName, label=some("R" & $ruleIdx), [("fillcolor", "3"), ("shape", "diamond"), ("style", "filled")]) # e.g. 1R5 
          .edge($state, quote reduceStateName, none[string](), [("style", "solid")]) # e.g. 1 -> "1R5"
    elif state in gt and nxt in gt[state]:
        discard gr.edge($state, $gt[state][nxt], some $nxt, [("style", "dashed")]) # e.g. 1 --left--> 2
    else:
      if nxt.kind == SymbolKind.ErrorS:
        discard gr.edge($state, $at[state][nxt].state, none[string](), [("style", "dotted")]) # no label on error 
      else:
        discard gr.edge($state, $at[state][nxt].state, some $nxt, [("style", "solid")]) # e.g. 1 --Token-->2
  discard gr.node($state, some ns) # add the state node


proc populateDotGraph*[T](gr: var Graph, itms: LALRItems[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) = 
  var ns = "State " & $state & "\\n\\l "
  # collapse same rule, diff lookahead into 1 entry 
  var ruleIdxToLookaheads : Table[int, seq[Symbol[T]]]
  # prevent duplicate edges
  var populatedTransitions : HashSet[Symbol[T]]
  for i in itms:
    let ruleIdx = findRuleIdx(g, i.rule)
    if ruleIdx notin ruleIdxToLookaheads:
      ruleIdxToLookaheads[ruleIdx] = @[i.ahead]
    else:
      ruleIdxToLookaheads[ruleIdx].add i.ahead 
  for i in itms:
    let ruleIdx = findRuleIdx(g, i.rule)
    if ruleIdx notin ruleIdxToLookaheads:
      continue 
    ns.add $ruleIdx
    ns.add " "
    populateRuleStringWithDot(ns, i.rule.left.nonTerm, i.rule.right, i.pos)
    ns.add " ["
    for j,ahead in ruleIdxToLookaheads[ruleIdx]:
      ns.add $ahead
      if j < ruleIdxToLookaheads[ruleIdx].len-1:
        ns.add ","
    ns.add "]"
    ns.add "\\l"
    let nxt = next(i)
    if nxt in populatedTransitions:
      continue 
    populatedTransitions.incl nxt
    if nxt == End[T]():
        # e.g. 1R5
        let reduceStateName = $state & "R" & $ruleIdx
        # need to quote 1R5 otherwise doesn't work correctly
        discard gr.node(quote reduceStateName, label=some("R" & $ruleIdx), [("fillcolor", "3"), ("shape", "diamond"), ("style", "filled")]) # e.g. 1R5 
          .edge($state, quote reduceStateName, none[string](), [("style", "solid")]) # e.g. 1 -> "1R5"
    elif state in gt and nxt in gt[state]:
        discard gr.edge($state, $gt[state][nxt], some $nxt, [("style", "dashed")]) # e.g. 1 --left--> 2
    else:
      if nxt.kind == SymbolKind.ErrorS:
        discard gr.edge($state, $at[state][nxt].state, none[string](), [("style", "dotted")]) # no label on error 
      else:
        discard gr.edge($state, $at[state][nxt].state, some $nxt, [("style", "solid")]) # e.g. 1 --Token-->2
        
    ruleIdxToLookaheads.del(ruleIdx) 
  discard gr.node($state, some ns) # add the state node
      
proc `$`*[T](r: Symbol[T]) : string = 
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

proc `$`*[T](rule: Rule[T]) : string = 
  result.add rule.left.nonTerm
  result.add " -> "
  for i,r in rule.right:
    result.add $r
    if i < rule.right.len-1:
      result.add " " 

proc `$`*[T](ft: FollowTable[T]): string =
  result = "\n--------\n"
  for i, itms in ft:
    result = result & $i & ":" & $itms & "\n"
  result = result & "--------\n"

proc `$`*[T](g: Grammar[T]): string = 
    result = "\n---- Grammar ----\n"
    for i,r in g.rules:
      result.add "\t"
      result.add $i
      result.add " " 
      result.add $r
      result.add "\n"
    
proc `$`*[T](at: ActionTable[T]): string =
  result = "\nActionTable:\n--------\n"
  for s, row in at:
    result = result & $s & ":" & $row & "\n"
  result = result & "--------\n"

proc `$`*[T](gt: GotoTable[T]): string =
  result = "\nGotoTable:\n--------\n"
  for s, row in gt:
    result = result & $s & ":" & $row & "\n"
  result = result & "--------\n"
