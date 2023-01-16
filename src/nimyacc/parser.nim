import tables
import strutils

import patty

import lexer
import parsetypes
import debuginfo
import std/options

# TODO would be nice to give user a way to handle parser error, but would need to research on how that even works.

proc `$`*[T](i: ActionTableItem[T]): string =
  match i:
    Shift(state: s):
      return "Shift(" & $s & ")"
    Reduce(rule: r):
      return "Reduce(" & $r & ")"
    Accept:
      return "Accept"
    Error:
      return "Error"

proc Shift*[T](state: parsetypes.State): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Shift, state: state)

proc Reduce*[T](rule: Rule[T]): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Reduce, rule: rule)

proc Accept*[T](): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Accept)

proc Error*[T](): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Error)


proc `$`*[T, S](pt: ParseTree[T, S], indent: int = 0): string =
  match pt:
    Terminal(token: t):
      result = "  ".repeat(indent) & $t & "\n"
    NonTerminal(rule: r, tree: t):
      result = "  ".repeat(indent) & "rule: " & $r & "\n"
      for n in t:
        result = result & `$`(n, indent + 1)
    ErrorNode():
      result = " ".repeat(indent) & "errorNode\n"

proc add[T](parser: var Parser[T], s: parsetypes.State) =
  parser.stack.add(s)

proc push[T](parser: var Parser[T], s: parsetypes.State) =
  parser.add(s)

proc pop[T](parser: var Parser[T]): parsetypes.State =
  return parser.stack.pop

proc top[T](parser: Parser[T]): parsetypes.State =
  return parser.stack[parser.stack.high]

proc nextChar[LS,T,S](lexer: var NimlLexer[LS,T], token: var T, symbol: var Symbol[S]) =
  try:
    token = lexer.lexNext
    symbol = TermS[S](token.kind)
  except NimlEOFError:
    symbol = End[S]()
  except:
    raise # TODO eliminate exceptions from lexer. replace with error type. 

proc parseImpl*[LS,T, S](parser: var Parser[S],
                      lexer: var NimlLexer[LS,T]): ParseTree[T, S] =
  var 
    tree: seq[ParseTree[T, S]] = @[]
    token: T
    symbol: Symbol[S]
    prevErrorLookahead : Option[Symbol[S]]
  
  lexer.nextChar(token, symbol)
  while true:
    when defined(nimytrace):
      echo "\nparser stack:" & $parser.stack
      echo "read token:" & $symbol
    var action: ActionTableItem[S]
    
    if symbol notin parser.table.action[parser.top]:
      action = Error[S]()
    else:
      action = parser.table.action[parser.top][symbol]
    
    when defined(nimytrace):
      echo "action: " & $action
    
    case action.kind
    of ActionTableItemKind.Shift:
      let s = action.state
      tree.add(Terminal[T, S](token))
      lexer.nextChar(token, symbol)
      parser.push(s)
    of ActionTableItemKind.Reduce:
      let r = action.rule
      let reseted = tree[^r.lenWithoutEmpty..^1]
      for i in 0..<r.lenWithoutEmpty:
        discard parser.pop
        discard tree.pop
      tree.add(NonTerminal[T, S](r, reseted))
      parser.push(parser.table.goto[parser.top][r.left])
    of ActionTableItemKind.Accept:
      when defined(nimytrace):
        if tree.len == 1:
          echo tree[0]
        else:
          echo tree
      doAssert tree.len == 1, "Error, parsing result is wrong."
      return NonTerminal[T, S](rule = Rule[S](), tree =tree)
    of ActionTableItemKind.Error:
      if prevErrorLookahead.isSome and parser.stack.len <= 1:
        # infinite loop detection: for the case when there is 
        # no possible action for the lookahead token we would 
        # be stuck. discarding tokens might help, so we will 
        # do that. this guarantees progress. this works because
        # any shift/reduce action puts something on the stack,
        # and accept would have returned. 
        when defined(nimytrace):
          echo "infinite loop detected: no progress possible with an empty stack and lookahead, discarding lookahead to try again. lookahead=" & $prevErrorLookahead.get 
        # sanity check
        doAssert prevErrorLookahead.get == symbol
        lexer.nextChar(token, symbol)

      parser.hasError = true 
      # are we out of luck (tokens)? 
      if symbol == End[S]():
        # we will just return ErrorNode as the parse tree. 
        return ErrorNode[T,S]()
      let errSym = ErrorS[S]()

      prevErrorLookahead = some symbol  
      # pop the stack until we reach state in which the 
      # action for the error token is shift
      # while parser.stack.len > 1 and errSym notin parser.table.action[parser.top]:
      while parser.stack.len > 1 and (errSym notin parser.table.action[parser.top] or parser.table.action[parser.top][errSym].kind != ActionTableItemKind.Shift):
        when defined(nimytrace):
          if errSym notin parser.table.action[parser.top]:
            echo "no action for error symbol. discarding stack.top=" & $parser.top()
          else:
            echo "action for error symbol is not Shift but is " & $parser.table.action[parser.top][errSym] & ". discarding stack.top=" & $parser.top()
        discard parser.pop()
        discard tree.pop()

      # we could have error token show up in the initial state
      # so we check if we can shift the error token first. 
      if errSym in parser.table.action[parser.top]:
        if parser.table.action[parser.top][errSym].kind == ActionTableItemKind.Shift:
          # shift the error symbol
          when defined(nimytrace):
            echo "adding ErrorNode to parse tree"
          tree.add(ErrorNode[T,S]())
          parser.push(parser.table.action[parser.top][errSym].state)
          # skip lookaheads until a state is reached that 
          # has a non-error action on the lookahead
          while symbol != End[S]() and 
            (symbol notin parser.table.action[parser.top] or 
              parser.table.action[parser.top][symbol].kind == ActionTableItemKind.Error):
              when defined(nimytrace):
                echo "discarding lookahead=" & $token
              lexer.nextChar(token, symbol)
          if symbol == End[S]():
            return ErrorNode[T,S]()
      # it is just a syntax error if "error" is not in the rule. do not try 
      # to complicate things further say by discarding lookaheads and see if 
      # we can eventually parse something. more sane to bail at this point. 
      return ErrorNode[T,S]()
        

proc newParser*[T](t: ParsingTable[T]): Parser[T] =
  result = Parser[T](stack: @[0], table: t, provisionalToksCnt: 0, hasError:false)
  result.init()

proc init*[T](p: var Parser[T]) =
  # annoyingly, "reset" is a built-in proc that sets something to its default state,
  # so we can't name this proc "reset" because if we do the system.reset get called
  # and bad things happen. will keep calling this "init". 
  p.stack = @[0]
