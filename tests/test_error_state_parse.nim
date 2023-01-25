import unittest
import nimyacc
import state_parser
import common

test "test error":
  var s: LexerState
  var uas: UserActionState

  var lexer = testStateLex.newWithString(s, "error if test + 1 then { true } else { 2 * ( test + 3 ) }")
  var parser = testStatePar.newParser()
  discard parser.parse_testStatePar(lexer,uas)
  doAssert parser.hasError
