import unittest
import patty

import nimly
import std/options

genStringMatcher testLex[int,string]:
  r"if":
    yield input.substr(oldpos, pos-1)
  r"else":
    yield "acc"
  r"\s":
    discard 

test "sanity":
  var iter = testLex("""if
else if
else""")
  var state = 42
  var actual : seq[string]
  for x in iter(state):
    actual.add x
  check @["if", "acc", "if", "acc"] == actual

test "test macro niml (if/else)":
  var testLexer = testLex.newWithString(42, """if
else if
else""")
  var ret: seq[string] = @[]
  for s in testLexer.lexIter():
    ret.add(s)
  check ret == @["if", "acc", "if", "acc"]

test "is empty":
  var testLexer = testLex.newWithString(42, "")
  check testLexer.isEmpty()

test "calling lexNext on empty throws NimlEOFError":
  var testLexer = testLex.newWithString(42, "")
  check testLexer.isEmpty()
  expect(NimlEOFError):
    discard testLexer.lexNext()

test "lexNext":
  var testLexer = testLex.newWithString(42, """if
else if
else""")
  var ret: seq[string] = @[]
  while not testLexer.isEmpty():
    ret.add testLexer.lexNext()

  check ret == @["if", "acc", "if", "acc"]
  check testLexer.isEmpty
  expect(NimlEOFError):
    discard testLexer.lexNext()