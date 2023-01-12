import tables
import strutils

import patty

import lextypes
import lexer
import parsetypes
import debuginfo

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

proc add[T](parser: var Parser[T], s: parsetypes.State) =
  parser.stack.add(s)

proc push[T](parser: var Parser[T], s: parsetypes.State) =
  parser.add(s)

proc pop[T](parser: var Parser[T]): parsetypes.State =
  return parser.stack.pop

proc top[T](parser: Parser[T]): parsetypes.State =
  return parser.stack[parser.stack.high]

proc parseImpl*[T, S](parser: var Parser[S],
                      lexer: var NimlLexer[T]): ParseTree[T, S] =
  var tree: seq[ParseTree[T, S]] = @[]
  var token: T
  var symbol: Symbol[S]
  if lexer.isEmpty:
    symbol = End[S]()
  else:
    token  = lexer.lexNext
    symbol = TermS[S](token.kind)
  while true:
    when defined(nimydebug):
      echo "parser stack:" & $parser.stack
      echo "read token:" & $symbol
    var action: ActionTableItem[S]
    try:
      action = parser.table.action[parser.top][symbol]
    except KeyError:
      var msg: string = "Unexpected token " & $symbol & " is passed."
      if symbol.kind == SymbolKind.End:
        msg = "Unexpected lexer stops (EOF). Cannot parse whole the tokens lexer passes."
      else:
        try:
          msg = msg & "\ntoken: " & $token
        except:
          discard
      raise newException(NimyActionError, msg)
    except:
      raise
    when defined(nimydebug):
      echo "action: " & $action
    case action.kind
    of ActionTableItemKind.Shift:
      let s = action.state
      tree.add(Terminal[T, S](token))
      try:
        token = lexer.lexNext
        symbol = TermS[S](token.kind)
      except NimlEOFError:
        symbol = End[S]()
      except:
        raise
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
      doAssert false, "Error, Must be implemented."
  assert false, "Something wrong with parser."

proc newParser*[T](t: ParsingTable[T]): Parser[T] =
  result = Parser[T](stack: @[0], table: t, provisionalToksCnt: 0, errState: Normal)
  result.init()

proc init*[T](p: var Parser[T]) =
  p.stack = @[0]
