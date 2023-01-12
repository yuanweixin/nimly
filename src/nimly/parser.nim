import tables
import strutils

import patty

import lextypes
import lexer
import parsetypes
import debuginfo

# TODO turn parseImpl return type into option type and the generated code should return option type as well. 
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

proc nextChar[T,S](lexer: var NimlLexer[T], token: var T, symbol: var Symbol[S]) =
  try:
    token = lexer.lexNext
    symbol = TermS[S](token.kind)
  except NimlEOFError:
    symbol = End[S]()
  except:
    raise
  

proc parseImpl*[T, S](parser: var Parser[S],
                      lexer: var NimlLexer[T]): ParseTree[T, S] =
  var tree: seq[ParseTree[T, S]] = @[]
  var token: T
  var symbol: Symbol[S]
  if lexer.isEmpty:
    symbol = End[S]()
  else:
    token  = lexer.lexNext # TODO lexNext should yield End on EOF then raise exception if called again. 
    symbol = TermS[S](token.kind)
  while true:
    when defined(nimytrace):
      echo "parser stack:" & $parser.stack
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
      when defined(nimydebug):
        if tree.len == 1:
          echo tree[0]
        else:
          echo tree
      doAssert tree.len == 1, "Error, parsing result is wrong."
      return NonTerminal[T, S](rule = Rule[S](), tree =tree)
    of ActionTableItemKind.Error:
      # are we out of luck (tokens)? 
      if symbol == End[S]():
        raise newException(Exception, "Unexpected EOF during parse")
      let errSym = ErrorS[S]()

      # pop the stack until we reach state in which the 
      # action for the error token is shift
      # while parser.stack.len > 1 and errSym notin parser.table.action[parser.top]:
      while parser.stack.len > 1 and (errSym notin parser.table.action[parser.top] or parser.table.action[parser.top][errSym].kind != ActionTableItemKind.Shift):
        discard parser.stack.pop()

      # we could have error token show up in the initial state
      # so we check if we can shift the error token first. 
      if parser.table.action[parser.top][errSym].kind == ActionTableItemKind.Shift:
        # shift the error symbol
        tree.add(ErrorNode[T,S]())
        parser.push(parser.table.action[parser.top][errSym].state)
        while symbol != End[S]() and 
          (symbol notin parser.table.action[parser.top] or 
            parser.table.action[parser.top][symbol].kind == ActionTableItemKind.Error):
            lexer.nextChar(token, symbol)
        if symbol == End[S]():
          raise newException(Exception, "Unexpected EOF during parse")
        continue 
      else: # discarded all context. 
        continue 

proc newParser*[T](t: ParsingTable[T]): Parser[T] =
  result = Parser[T](stack: @[0], table: t, provisionalToksCnt: 0, errState: Normal)
  result.init()

proc init*[T](p: var Parser[T]) =
  p.stack = @[0]
