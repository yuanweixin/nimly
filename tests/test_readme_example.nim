import unittest
import patty
import strutils
import options
import nimyacc
import common

## variant is defined in patty
variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE

genStringMatcher testLex[LexerState,MyToken]:
  r"\(":
    yield LPAREN()
  r"\)":
    yield RPAREN()
  r"\+":
    yield PLUS()
  r"\*":
    yield MULTI()
  r"\d":
    yield NUM(parseInt(input.substr(oldpos, pos-1)))
  r"\.":
    yield DOT()
  r"\s":
    discard

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
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 42 * 101010")
  var
    ret: seq[MyTokenKind] = @[]

  while not testLexer.isEmpty():
    ret.add testLexer.lexNext().token.kind
    
  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.MULTI,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM]

test "test Parser 1":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 42 * 101010")

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "1 + [42 * 101010]"

  testLexer = testLex.newWithString(s, "1 + 42 * 1010")

  parser.init()
  check parser.parse_testPar(testLexer) == some "1 + [42 * 1010]"

test "test Parser 2":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 42 * 1.01010")

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "1 + [42 * 1.01010]"

  testLexer = testLex.newWithString(s, "1. + 4.2 * 101010")

  parser.init()
  check parser.parse_testPar(testLexer) == some "1. + [4.2 * 101010]"

test "test Parser 3":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "(1 + 42) * 1.01010")

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "[(1 + 42) * 1.01010]"


