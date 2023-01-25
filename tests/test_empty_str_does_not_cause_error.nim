import unittest
import patty
import nimyacc
import options
import common

variant Token:
  CHARS(val: string)

genStringMatcher testLex[LexerState, Token]:
  r"\w+":
    yield CHARS(input.substr(oldPos, pos-1))
  r"\s":
    discard

nimy testPar[Token, UserActionState]:
  top[seq[string]]:
    word{}:
      return $1
  word[string]:
    CHARS:
      return ($1).val


test "parser works":
  var s: LexerState
  var uas: UserActionState
  var testLexer = testLex.newWithString(s, "This is a test")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer, uas) == some @["This", "is", "a", "test"]

test "empty string does not cause error":
  var s: LexerState
  var uas: UserActionState

  var testLexer = testLex.newWithString(s, "")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer, uas).get.len == 0
