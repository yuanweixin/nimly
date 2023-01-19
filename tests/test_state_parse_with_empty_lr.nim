import unittest
import nimyacc
import options
import state_parser_with_empty_lr

test "test state":
  var lexer = testStateLex.newWithString(42, "if test + 1 then { true } else { 2 * ( test + 3 ) }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer) == some "IF(test+1)THEN{true}ELSE{(2*(test+3))}"

test "test state with empty":
  var lexer = testStateLex.newWithString(42, "if test + 1 then { true }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer) == some "IF(test+1)THEN{true}"

test "test state with empty2":
  var lexer = testStateLex.newWithString(42, "if then { true }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer) == some "IF()THEN{true}"
