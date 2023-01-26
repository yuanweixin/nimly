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

# Terminals return their (s,e) as a singleton list. 
# Nonterminals returns the concat of their children's lists, plus the start of $1 and end of $last 
# which should match what this parent node's parent see anyway. 
# 
# This captures the start,end ranges that would have been seen during an 
nimy testPar[MyToken]:
  top[seq[(int,int)]]:
    plus:
      let s = nimyacctree.getStartPos(1)
      let e = nimyacctree.getEndPos(1)
      return $1 & @[(s,e)]
  plus[seq[(int,int)]]:
    plus PLUS plus:
      let s = nimyacctree.getStartPos(1)
      let e = nimyacctree.getEndPos(3)
      let ps = nimyacctree.getStartPos(2)
      let pe = nimyacctree.getEndPos(2)
      return $1 & @[(ps,pe)] & $3 & @[(s,e)]
    mult:
      let s = nimyacctree.getStartPos(1)
      let e = nimyacctree.getEndPos(1)
      return $1 & @[(s,e)]
  mult[seq[(int,int)]]:
    mult MULTI mult:
      let mults = nimyacctree.getStartPos(2)
      let multe = nimyacctree.getEndPos(2)
      let s = nimyacctree.getStartPos(1)
      let e = nimyacctree.getEndPos(3)
      return $1 & @[(mults, multe)] & $3 & @[(s,e)]
    num:
      let s = nimyacctree.getStartPos(1)
      let e = nimyacctree.getEndPos(1)
      return $1 & @[(s,e)]
  num[seq[(int,int)]]:
    NUM:
      let s = nimyacctree.getStartPos(1)
      let e = nimyacctree.getEndPos(1)
      return @[(s,e)]

# input " 2+3+4" produces the parse tree below. Number on right is order of addition to result. 
# 
# rule: __Start__ -> top, (0, 5)
#   rule: top -> plus, (1, 5) 14
#     rule: plus -> plus 0 plus, (1, 5) 13 
#       rule: plus -> mult, (1, 1) 3 
#         rule: mult -> num, (1, 1) 2  
#           rule: num -> 2, (1, 1) 1
#             (kind: NUM, val: 2) , (1, 1)  
#       (kind: PLUS) , (2, 2) 4 
#       rule: plus -> plus 0 plus, (3, 5) 12 
#         rule: plus -> mult, (3, 3) 7 
#           rule: mult -> num, (3, 3) 6 
#             rule: num -> 2, (3, 3) 5 
#               (kind: NUM, val: 3) , (3, 3)
#         (kind: PLUS) , (4, 4) 8 
#         rule: plus -> mult, (5, 5)  11 
#           rule: mult -> num, (5, 5) 10 
#             rule: num -> 2, (5, 5) 9 
#               (kind: NUM, val: 4) , (5, 5)
# 
# 
test "parsing":
  var s: LexerState
  var testLexer = testLex.newWithString(s, " 2+3+4")
  var parser = testPar.newParser()
  let expected = some @[(1,1), (1,1), (1,1), (2,2), (3,3), (3,3), (3,3), (4,4), (5,5), (5,5), (5,5), (3,5), (1,5), (1,5)]
  check parser.parse_testPar(testLexer) == expected
