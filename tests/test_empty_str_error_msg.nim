import unittest
import strutils
import patty
import nimyacc
import options
import common

variant Token:
  CHARS(val: string)
  IGNORE

genStringMatcher testLex[LexerState,Token]:
  r"\w+":
    yield CHARS(input.substr(oldpos, pos-1))
  r"\s":
    discard

nimy testPar[Token, UserActionState]:
  top[seq[string]]:
    word word{}:
      return @[$1] & $2
  word[string]:
    CHARS:
      return ($1).val

test "parser works":
  var s: LexerState
  var uas: UserActionState

  var testLexer = testLex.newWithString(s, "This is a test")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer,uas) == some @["This", "is", "a", "test"]

test "empty string":
  var uas: UserActionState
  var s: LexerState
  var testLexer = testLex.newWithString(s, "")
  var parser = testPar.newParser()
  let actual = parser.parse_testPar(testLexer,uas)
  check parser.hasError
  check actual.isNone