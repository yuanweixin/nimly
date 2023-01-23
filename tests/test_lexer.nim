import unittest
import patty

import nimyacc
import options
import common

genStringMatcher testLex[LexerState,string]:
  r"if":
    yield input.substr(oldpos, pos-1)
  r"else":
    yield "acc"
  r"\s":
    discard 

test "sanity":
  var s: LexerState
  var iter = testLex("""if
else if
else""")
  var actual : seq[string]
  for x in iter(s):
    actual.add x
  check @["if", "acc", "if", "acc"] == actual

test "test (if/else)":
  var s: LexerState
  var testLexer = testLex.newWithString(s, """if
else if
else""")
  var ret: seq[string] = @[]
  while not testLexer.isEmpty():
    ret.add testLexer.lexNext().token
  check ret == @["if", "acc", "if", "acc"]

test "is empty":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "")
  check testLexer.isEmpty()

test "is empty lexNext":
  var s: LexerState
  var testLexer = testLex.newWithString(s, "")
  check testLexer.lexNext().kind == Eof

test "normal usage":
  var s : LexerState
  var testLexer = testLex.newWithString(s, """if
else if
else""")
  var ret: seq[string] = @[]
  while not testLexer.isEmpty():
    ret.add testLexer.lexNext().token

  check ret == @["if", "acc", "if", "acc"]
  check testLexer.isEmpty
  check testLexer.lexNext().kind == Eof

test "jammed":
  var s : LexerState
  var testLexer = testLex.newWithString(s, "fffft")
  check testLexer.lexNext().kind == Jammed
  check testLexer.lexNext().kind == Jammed
  check testLexer.lexNext().kind == Jammed
  check testLexer.isEmpty()