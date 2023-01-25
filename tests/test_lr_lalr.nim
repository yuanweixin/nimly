import unittest
import options
import nimyacc
import parser_415
import common

test "test lalr":
  var s : LexerState
  var lexer415 = lex415.newWithString(s, "left")
  var parser = psr415LALR.newParser()
  check parser_415.parse_psr415LALR(parser, lexer415) == some "left"
