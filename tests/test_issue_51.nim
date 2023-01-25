import unittest

import patty
import nimyacc
import common

variant MyToken:
  tSYM(str: string)

genStringMatcher lexer1[LexerState, MyToken]:
  r"([^ \t\r\C\L]|\C\L)+":  # ugly hack to get around lexim's inability to put \n into a class!
    yield tSYM(input.substr(oldpos, pos-1))

genStringMatcher lexer2[LexerState, MyToken]:
  r"\S+":
    yield tSYM(input.substr(oldpos, pos-1))



test "test [^...] in regex":
  var s : LexerState
  
  var testLexer = lexer1.newWithString(s, "xxx<<1")
  var ret: seq[LexerOutput[MyToken]] = @[]
  while not testLexer.isEmpty():
    ret.add testLexer.lexNext()
  let expected = @[LexerOutput[MyToken](kind: Token, token: tSYM("xxx<<1"), startPos: 0, endPosExcl: 6)]
  check expected == ret 

test "test [^...] in regex (jammed)":
  var s : LexerState
  var testLexer = lexer1.newWithString(s, "xxx << 1")
  check testLexer.lexNext().token.str == "xxx"
  check testLexer.lexNext().kind == Jammed
  check testLexer.lexNext().kind == Jammed
  check testLexer.lexNext().kind == Jammed

test r"test \S in regex":
  var s: LexerState
  var testLexer = lexer2.newWithString(s, "xxx<<1")
  var ret: seq[MyToken] = @[]
  while not testLexer.isEmpty():
    ret.add testLexer.lexNext().token
  check ret == @[tSYM("xxx<<1")]

test r"test \S in regex (jammed)":
  var s : LexerState
  # this is no match. should not yield any token. 
  var testLexer = lexer2.newWithString(s, "xxx << 1")
  check testLexer.lexNext.token.str == "xxx"
  check testLexer.lexNext().kind == Jammed
  check testLexer.lexNext().kind == Jammed
  check testLexer.lexNext().kind == Jammed

