import unittest
import strutils
import patty
import nimly
import std/options

variant Token:
  CHARS(val: string)
  IGNORE

niml testLex[Token]:
  r"\w+":
    return CHARS(token.token)
  r"\s":
    return IGNORE()

nimy testPar[Token]:
  top[seq[string]]:
    word word{}:
      return @[$1] & $2
  word[string]:
    CHARS:
      return ($1).val

test "parser works":
  var testLexer = testLex.newWithString("This is a test")
  testLexer.ignoreIf = proc(r: Token): bool = r.kind == TokenKind.IGNORE
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some @["This", "is", "a", "test"]

test "empty string":
  var testLexer = testLex.newWithString("")
  testLexer.ignoreIf = proc(r: Token): bool = r.kind == TokenKind.IGNORE
  var parser = testPar.newParser()
  let actual = parser.parse_testPar(testLexer)
  check parser.hasError
  check actual.isNone