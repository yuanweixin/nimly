import unittest
import options
import nimyacc
import parser_415
import parser_415_lr
import common

test "test lalr":
  var s : LexerState
  var lexer415 = lex415.newWithString(s, "left")
  var parser = psr415LALR.newParser()
  check parser_415.parse_psr415LALR(parser, lexer415) == some "left"

test "test lr":
  var s: LexerState
  var lexer415 = lex415.newWithString(s,"left")
  var parser = psr415LR.newParser()
  check parser_415_lr.parse_psr415LR(parser, lexer415) == some "left"
