import unittest
import patty
import strutils

import nimyacc
import options
import common

variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  IGNORE

genStringMatcher testLex[LexerState,MyToken]:
  r"\+":
    yield PLUS()
  r"\*":
    yield MULTI()
  r"\d*":
    yield NUM(parseInt(input.substr(oldpos, pos-1)))
  r"\s":
    discard

nimy testPar[MyToken, UserActionState]:
  top[string]:
    plus:
      uastate.dummy = $1
      return $($1)
  plus[int]:
    plus PLUS plus:
      return $1  + $3
    mult:
      return $1
  mult[int]:
    mult MULTI mult:
      return $1 * $3
    num:
      return $1
  num[int]:
    NUM:
      return ($1).val

test "use user action state as counter":
  var s: LexerState
  var uas: UserActionState

  var testLexer = testLex.newWithString(s, "1 + 2 * 3")
  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer,uas) == some "7"
  check uas.dummy == 7
