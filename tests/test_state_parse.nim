import unittest
import nimyacc
import state_parser
import options
import common

test "test state":
  var s: LexerState
  var uas: UserActionState

  var lexer = testStateLex.newWithString(s, "if test + 1 then { true } else { 2 * ( test + 3 ) }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer, uas) == some "IF(test+1)THEN{true}ELSE{(2*(test+3))}"
