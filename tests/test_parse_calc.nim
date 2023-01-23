import unittest
import patty
import strutils

import nimyacc
import options
import common

variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  IGNORE

genStringMatcher testLex[LexerState,MyToken]:
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
      return $($1)
  plus[int]:
    plus PLUS plus:
      return $1  + $3
    mult:
      return $1
  mult[int]:
    mult MULTI mult:
      return $1 * $3
    num:
      return $1
  num[int]:
    NUM:
      return ($1).val

test "lexer":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 2 * 3")
  var
    ret: seq[MyTokenKind] = @[]
  while not testLexer.isEmpty():
    ret.add testLexer.lexNext().token.kind
  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.MULTI, MyTokenKind.NUM]

test "parsing":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 2 * 3")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "7"
