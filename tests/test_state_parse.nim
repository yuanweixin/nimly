import unittest
import nimly
import state_parser
import std/options

test "test state":
  var lexer = testStateLex.newWithString(42, "if test + 1 then { true } else { 2 * ( test + 3 ) }")
  var parser = testStatePar.newParser()
  check parser.parse_testStatePar(lexer) == some "IF(test+1)THEN{true}ELSE{(2*(test+3))}"
