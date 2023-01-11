import parsetypes


proc `$`*[T](s: SetOfLRItems[T]): string =
  result = "CanonicalCollection:\n--------\n"
  for i, itms in s:
    result = result & $i & ":" & $itms & "\n"
  result = result & "--------\n"

proc `$`*[T](i: LALRItem[T]) : string = 
  result.add i.rule.left.nonTerm
  result.add " -> "
  for pos, r in i.rule.right:
    if pos == i.pos:
      result.add ". "
    result.add $r
    result.add " "
  if i.pos == i.rule.right.len:
    result.add ". "
  result.add " ["
  result.add $i.ahead
  result.add "]"

proc `$`*[T](i: LRItem[T]) : string = 
    result.add i.rule.left.nonTerm
    result.add " -> "
    for pos, r in i.rule.right:
        if pos == i.pos:
            result.add ". "
        result.add $r
        result.add " "
    if i.pos == i.rule.right.len:
        result.add ". "

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
        result.add ", shift and go to state "
        result.add $at[state][nxt].state


proc lalrItemsToString*[T](itms: LALRItems[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) : string = 
  for r in itms:
    result.add "\t"
    result.add $r
    result.add "\n"
  result.add "\n"
  for r in itms:
    result.add lalrItemToString(r, g, at, gt, state)
    result.add "\n"

proc lrItemsToString*[T](itms: LRItems[T], g: Grammar[T], at: var ActionTable[T], gt: var GotoTable[T], state: State) : string = 
    for r in itms:
        result.add "\t"
        result.add $r
        result.add "\n"
    result.add "\n"
    for r in itms:
        result.add lrItemToString(r, g, at, gt, state)
        result.add "\n"

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
    for r in g.rules:
      result.add "\t"
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
