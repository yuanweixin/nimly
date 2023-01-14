import unittest
import std/options
import nimyacc
import parser_415
import parser_415_lr

## to check LR:CONFLICT in -d:nimydebug mode

test "test lalr":
  var lexer415 = lex415.newWithString(42, "left")
  var parser = psr415LALR.newParser()
  check parser_415.parse_psr415LALR(parser, lexer415) == some "left"

test "test lr":
  var lexer415 = lex415.newWithString(42,"left")
  var parser = psr415LR.newParser()
  check parser_415_lr.parse_psr415LR(parser, lexer415) == some "left"
