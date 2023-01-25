import unittest
import nimyacc
import state_parser_with_empty
import options
import common

test "test state":
  var s: LexerState
  var uas: UserActionState

  var lexer = testStateLex.newWithString(s,"if test + 1 then { true } else { 2 * ( test + 3 ) }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer, uas) == some "IF(test+1)THEN{true}ELSE{(2*(test+3))}"

test "test state with empty":
  var s: LexerState
  var uas: UserActionState
  var lexer = testStateLex.newWithString(s,"if test + 1 then { true }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer, uas) == some "IF(test+1)THEN{true}"

test "test state with empty2":
  var s: LexerState
  var uas: UserActionState
  var lexer = testStateLex.newWithString(s,"if then { true }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer, uas) == some "IF()THEN{true}"
