import unittest
import patty
import strutils
import std/options
import nimly

## variant is defined in patty
variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE

niml testLex[MyToken]:
  r"\(":
    return LPAREN()
  r"\)":
    return RPAREN()
  r"\+":
    return PLUS()
  r"\*":
    return MULTI()
  r"\d":
    return NUM(parseInt(token.token))
  r"\.":
    return DOT()
  r"\s":
    return IGNORE()

nimy testPar[MyToken]:
  top[string]:
    plus:
      return $1

  plus[string]:
    mult PLUS plus:
      return $1 & " + " & $3

    mult:
      return $1

  mult[string]:
    num MULTI mult:
      return "[" & $1 & " * " & $3 & "]"

    num:
      return $1

  num[string]:
    LPAREN plus RPAREN:
      return "(" & $2 & ")"

    ## float (integer part is 0-9) or integer
    NUM DOT[] NUM{}:
      result = ""
      # type of `($1).val` is `int`
      result &= $(($1).val)
      if ($2).len > 0:
        result &= "."
      # type of `$3` is `seq[MyToken]` and each elements are NUM
      for tkn in $3:
        # type of `tkn.val` is `int`
        result &= $(tkn.val)

test "test Lexer":
  var testLexer = testLex.newWithString("1 + 42 * 101010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var
    ret: seq[MyTokenKind] = @[]

  for token in testLexer.lexIter:
    ret.add(token.kind)

  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.MULTI,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM]

test "test Parser 1":
  var testLexer = testLex.newWithString("1 + 42 * 101010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "1 + [42 * 101010]"

  testLexer.initWithString("1 + 42 * 1010")

  parser.init()
  check parser.parse_testPar(testLexer) == some "1 + [42 * 1010]"

test "test Parser 2":
  var testLexer = testLex.newWithString("1 + 42 * 1.01010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "1 + [42 * 1.01010]"

  testLexer.initWithString("1. + 4.2 * 101010")

  parser.init()
  check parser.parse_testPar(testLexer) == some "1. + [4.2 * 101010]"

test "test Parser 3":
  var testLexer = testLex.newWithString("(1 + 42) * 1.01010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "[(1 + 42) * 1.01010]"
