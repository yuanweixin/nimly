import unittest
import patty
import strutils
import macros
import std/math
import nimly

## variant is defined in patty
variant MyToken:
  ID 
  PLUS 
  LPAREN 
  RPAREN
  SEMI
  IGNORE
  
niml testLex[MyToken]:
  r"\(":
    return LPAREN()
  r"\)":
    return RPAREN()
  r"\+":
    return PLUS()
  r"ID":
    return ID()
  r";":
    return SEMI()
  r"\s":
    return IGNORE()

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
  var testLexer = testLex.newWithString(")ID")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var parser = testPar.newParser()
  discard parser.parse_testPar(testLexer) 

test "infinite loop does not occur":
  var testLexer = testLex.newWithString(")ID")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var parser = infiniteLoop.newParser()
  discard parser.parse_infiniteLoop(testLexer) 