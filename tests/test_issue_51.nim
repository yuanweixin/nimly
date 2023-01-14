import unittest

import patty
import nimly

variant MyToken:
  tSYM(str: string)

genStringMatcher lexer1[int, MyToken]:
  r"([^ \t\r\C\L]|\C\L)+":  # ugly hack to get around lexim's inability to put \n into a class!
    yield tSYM(input.substr(oldpos, pos-1))

genStringMatcher lexer2[int, MyToken]:
  r"\S+":
    yield tSYM(input.substr(oldpos, pos-1))

test "test [^...] in regex":
  var testLexer = lexer1.newWithString(42, "loloi<<1")
  var ret: seq[MyToken] = @[]
  for s in testLexer.lexIter:
    ret.add(s)
  check ret == @[tSYM("loloi<<1")]

test "test [^...] in regex (exception)":
  var testLexer = lexer1.newWithString(42, "loloi << 1")
  expect Exception:
    discard testLexer.lexNext

test r"test \S in regex (exception)":
  var testLexer = lexer2.newWithString(42, "loloi<<1")
  var ret: seq[MyToken] = @[]
  for s in testLexer.lexIter:
    ret.add(s)
  check ret == @[tSYM("loloi<<1")]

test r"test \S in regex (exception)":
  # this is no match. should not yield any token. 
  var testLexer = lexer2.newWithString(42, "loloi << 1")
  expect Exception:
    discard testLexer.lexNext

