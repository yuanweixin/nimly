import unittest
import std/options
import nimly
import parser_415
import parser_415_lr

## to check LR:CONFLICT in -d:nimydebug mode

test "test lalr":
  var lexer415 = lex415.newWithString("left")
  lexer415.ignoreIf = proc(r: MyTerm): bool = r.kind == MyTermKind.IGNORE
  var parser = psr415LALR.newParser()
  check parser_415.parse_psr415LALR(parser, lexer415) == some "left"

test "test lr":
  var lexer415 = lex415.newWithString("left")
  lexer415.ignoreIf = proc(r: MyTerm): bool = r.kind == MyTermKind.IGNORE
  var parser = psr415LR.newParser()
  check parser_415_lr.parse_psr415LR(parser, lexer415) == some "left"
