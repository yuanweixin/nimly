import unittest
import strutils
import patty
import nimly
import std/options

variant Token:
  CHARS(val: string)
  IGNORE

genStringMatcher testLex[int,Token]:
  r"\w+":
    yield CHARS(input.substr(oldpos, pos-1))
  r"\s":
    discard

nimy testPar[Token]:
  top[seq[string]]:
    word word{}:
      return @[$1] & $2
  word[string]:
    CHARS:
      return ($1).val

test "parser works":
  var testLexer = testLex.newWithString(42, "This is a test")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some @["This", "is", "a", "test"]

test "empty string":
  var testLexer = testLex.newWithString(42, "")
  var parser = testPar.newParser()
  let actual = parser.parse_testPar(testLexer)
  check parser.hasError
  check actual.isNone