import unittest
import nimly
import state_parser

test "test error":
  var lexer = testStateLex.newWithString(42, "error if test + 1 then { true } else { 2 * ( test + 3 ) }")
  var parser = testStatePar.newParser()
  discard parser.parse_testStatePar(lexer)
  doAssert parser.hasError
