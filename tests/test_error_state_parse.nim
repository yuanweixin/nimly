import unittest
import nimly
import state_parser

test "test error":
  var lexer = testStateLex.newWithString("error if test + 1 then { true } else { 2 * ( test + 3 ) }")
  lexer.ignoreIf = proc(r: StateToken): bool = r.kind == StateTokenKind.SIGNORE

  var parser = testStatePar.newParser()
  discard parser.parse_testStatePar(lexer)
  doAssert parser.hasError
