import unittest
import patty

import nimly
import std/options

variant Token:
  CHARS(val: string)

genStringMatcher testLex[int, Token]:
  r"\w+":
    yield CHARS(input.substr(oldPos, pos-1))
  r"\s":
    discard

nimy testPar[Token]:
  top[seq[string]]:
    word{}:
      return $1
  word[string]:
    CHARS:
      return ($1).val


test "parser works":
  var testLexer = testLex.newWithString(42, "This is a test")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer) == some @["This", "is", "a", "test"]

test "empty string does not cause error":
  var testLexer = testLex.newWithString(42, "")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer).get.len == 0
