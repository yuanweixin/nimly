import unittest
import patty
import strutils
import macros
import math
import nimyacc

## variant is defined in patty
variant MyToken:
  ID 
  PLUS 
  LPAREN 
  RPAREN
  SEMI
  IGNORE
  
genStringMatcher testLex[int, MyToken]:
  r"\(":
    yield LPAREN()
  r"\)":
    yield RPAREN()
  r"\+":
    yield PLUS()
  r"ID":
    yield ID()
  r";":
    yield SEMI()
  r"\s":
    discard

nimy testPar[MyToken]:
  exps[int]:
    exp:
      return 1 
    exps exp:
      return 1 
    error exp:
      return 1 
    
  exp[int]:
    ID: 
      return 1 
    exp PLUS exp:
      return 1 
    LPAREN exps RPAREN:
      return 1 
    LPAREN error RPAREN: 
    # note: if this is changed into 
    # a single 'error', infinite loop results 
    # because the lookahead token is never actually shifted
      return 1 

nimy infiniteLoop[MyToken]:
  exps[int]:
    exp:
      return 1 
    exps exp:
      return 1 
    error exp:
      return 1 
    
  exp[int]:
    ID: 
      return 1 
    exp PLUS exp:
      return 1 
    LPAREN exps RPAREN:
      return 1 
    error: 
    # infinite loop results 
    # because the lookahead token is never actually shifted
      return 1 

test "appel book example":
  # from modern compiler implementation in ML p.76
  var testLexer = testLex.newWithString(42, ")ID")
  var parser = testPar.newParser()
  discard parser.parse_testPar(testLexer) 
  check parser.hasError

test "infinite loop does not occur":
  var testLexer = testLex.newWithString(42, ")ID")
  var parser = infiniteLoop.newParser()
  discard parser.parse_infiniteLoop(testLexer) 
  check parser.hasError

test "2 errors at least 3 shifts apart, both get reported":
  var testLexer = testLex.newWithString(42, "(+) + (+) + ID")
  var cnt = 0
  proc onError(pos: int) = 
    inc cnt
  var parser = testPar.newParser(onError)
  discard parser.parse_testPar(testLexer) 
  check cnt == 2
  check parser.hasError

test "2 errors within 3 shifts, only 1 is reported":
  var testLexer = testLex.newWithString(42, "(+) + +) + ID")
  # stack, lookahead, action
  # 0, (, shift
  # 0 (, +, error! 
  # 0 (, +, shift error
  # 0 ( error, +, drop lookahead
  # 0 ( error, ), shift 
  # 0 ( error ), +, reduce 
  # 0 exp, +, shift
  # 0 exp +, +, error! 
  # 0 exp +, +, pop +, pop exp
  # 0, +, shift error
  # 0 error, +, drop lookahead
  # 0 error, ), drop lookahead
  # 0 error, +, drop lookahead
  # 0 error, ID, shift
  # 0 error ID, reduce 
  # 0 exps, $, accept 
  # 
  # there's only 2 shifts between the first and second error, so only 1 call to onError happens
  var cnt = 0
  proc onError(pos: int) = 
    inc cnt
  var parser = testPar.newParser(onError)
  discard parser.parse_testPar(testLexer) 
  check parser.hasError
  check cnt == 1
