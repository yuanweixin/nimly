import unittest
import patty
import strutils

import nimly
import std/options

variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  IGNORE

niml testLex[MyToken]:
  r"\+":
    return PLUS()
  r"\*":
    return MULTI()
  r"\d*":
    return NUM(parseInt(token.token))
  r"\s":
    return IGNORE()

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
  var testLexer = testLex.newWithString("1 + 2 * 3")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var
    ret: seq[MyTokenKind] = @[]
  for token in testLexer.lexIter:
    ret.add(token.kind)
  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.MULTI, MyTokenKind.NUM]

test "parsing":
  var testLexer = testLex.newWithString("1 + 2 * 3")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some "7"
