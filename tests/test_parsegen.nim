import unittest
import strutils
import options
import nimyacc
import common

type
  MyTokenKind{.pure.} = enum
    PLUS
    MULTI
    NUM
    IGNORE
  MyToken = object
    case kind: MyTokenKind
    of NUM:
      val: int
    else:
      discard

proc PLUS(): MyToken =
  return MyToken(kind: MyTokenKind.PLUS)
proc MULTI(): MyToken =
  return MyToken(kind: MyTokenKind.MULTI)
proc NUM(num: int): MyToken =
  return MyToken(kind: MyTokenKind.NUM, val: num)
proc IGNORE(): MyToken =
  return MyToken(kind: MyTokenKind.IGNORE)

genStringMatcher testLex[LexerState, MyToken]:
  r"\+":
    yield PLUS()
  r"\*":
    yield MULTI()
  r"\d*":
    yield NUM(parseInt(input.substr(oldpos, pos-1)))
  r"\s":
    discard

nimy testPar[MyToken]:
  top[string]:
    plus:
      return $1
  plus[string]:
    plus PLUS plus:
      return $1 & " + " & $3
    mult:
      return $1
  mult[string]:
    mult MULTI mult:
      return "(" & $1 & " * " & $3 & ")"
    num:
      return $1
  num[string]:
    NUM:
      return $(($1).val)

test "test 1":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 2 * 3")
  var
    ret: seq[MyTokenKind] = @[]
  while not testLexer.isEmpty():
    ret.add testLexer.lexNext().token.kind
  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.MULTI, MyTokenKind.NUM]

test "test 2":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 2 * 3")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "1 + (2 * 3)"
  testLexer = testLex.newWithString(s, "1 + 2 * 3")
  parser.init()
  check parser.parse_testPar(testLexer) == some "1 + (2 * 3)"
