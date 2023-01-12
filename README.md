---
title: nimly
---

[![github\_workflow](https://github.com/loloicci/nimly/workflows/test/badge.svg)](https://github.com/loloicci/nimly/actions?query=workflow%3Atest)
[![nimble](https://raw.githubusercontent.com/yglukhov/nimble-tag/master/nimble.png)](https://github.com/yglukhov/nimble-tag)

# LALR, SLR generator macro library in Nim. 

This library started life as a fork of [nimly](https://github.com/loloicci/nimly), which provided the core implementation of the SLR and LALR algorithms, as well as the macro dsl. Various code improvements and extensions of functionality were made. 

## Here's a list of the added functionalities: 
* Support for bison style %left, %right, %nonassoc declarations, as well as rule level %prec declarations to use in conflict resolution. 
* Output text description of the grammar, conflict count, parser automaton, when -d:nimydebug is defined. 
* Output dot file of the parser automaton, when -d:nimygraphviz is passed. 
* Ad hoc error recovery using error token, as described in Modern Compiler Implementation in ML, p.76. I believe this is similar to what yacc/bison does. Fwiw it is basically the algorithm described in [PLY documentation](https://www.dabeaz.com/ply/ply.html#ply_nn29). 

  ### WIP
* Replace the original lexer code by [lexim](https://github.com/yuanweixin/lexim) which is a high performance scanner library. The original lexer code ran slower, was implemented in a way that ran too long at compile time, and had a bug where it crashes if a token does not fit the buffer. Lexim encodes the dfa in a goto loop which is faster, calls out to an executable to process the dfa instead of using the compiler vm, and uses strings instead of buffer, so it doesn't suffer from these problems. 

# Possible future extensions

* Local error recovery strategies. 
  
  Error rules are invasive changes to the grammar and force the grammar writer to anticipate error sites. On the other hand, there's a whole class of error recovery techniques that rely on doing a bounded search of possible token insertions, deletions and change on the local parse context. For example, ml-yacc implements [burke fisher error repair](https://en.wikipedia.org/wiki/Burke%E2%80%93Fisher_error_repair). There's several other techniques. For a relatively recent list of references, see section 18.2.7 of [Parsing techniques: a practical guide](https://link.springer.com/book/10.1007/978-0-387-68954-8). It would be ideal for this library to provide at least 1 such automated error recovery option as an alternative to the error token. 

* Example-based error message specification

  The (state, input token, error message) data can be semi-automated as a series of (bad input, error message) that gets fed to the parser, and the state at which the parser error occur can be discovered during the parse. 

* Canonical LR with table compression

  This would give full LR(1) power with negligible overhead compared to LALR. This would also eliminate the class of "mysterious reduce/reduce" conflicts that can happen with LALR parsers. 

* Counterexample generation to debug grammar development 

  See [bison doc](https://www.gnu.org/software/bison/manual/html_node/Counterexamples.html) for a description of the feature. 

Usage
====

TODO code examples below are WIP and probably do not reflect current state 

macro nimy
----------

macro `nimy` makes a parser. The SLR/LALR table construction is done at compile time. The generated code simply creates a proc that, when called, invokes the parser engine on the given input, and use the parser table to derive the parse tree. The parse tree is then traversed post-order. During the traversal, the user-specified code blocks are run. 

```nim
## This makes a LexData object named myParser.
## first cloud is the top-level of the BNF.
## This lexer recieve tokens with type ``Token`` and token must have a value
## ``kind`` with type enum ``[TokenTypeName]Kind``.
## This is naturally satisfied when you use ``patty`` to define the token.
nimy myParser[Token]:
  ## the starting non-terminal
  ## the return type of the parser is ``Expr``
  top[Expr]:
    ## a pattern.
    expr:
      ## proc body that is used when parse the pattern with single ``expr``.
      ## $1 means first position of the pattern (expr)
      return $1

  ## non-terminal named ``expr``
  ## with returning type ``Expr``
  expr[Expr]:
    ## first pattern of expr.
    ## ``LPAR`` and ``RPAR`` is TokenKind.
    LPAR expr RPAR:
      return $2

    ## second pattern of expr.
    ## ``PLUS`` is TokenKind.
    expr PLUS expr
      return $2
```

You can use following EBNF functions:

-   `XXX[]`: Option (0 or 1 `XXX`). The type is `seq[xxx]` where `xxx`
    is type of `XXX`.
-   `XXX{}`: Repeat (0 or more `XXX`). The type is `seq[xxx]` where
    `xxx` is type of `XXX`.

Example of these is in next section.

Example
=======

`tests/test_readme_example.nim` is an easy example.

``` {.nim}
import unittest
import patty
import strutils

import nimly

## variant is defined in patty
variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE

niml testLex[MyToken]:
  r"\(":
    return LPAREN()
  r"\)":
    return RPAREN()
  r"\+":
    return PLUS()
  r"\*":
    return MULTI()
  r"\d":
    return NUM(parseInt(token.token))
  r"\.":
    return DOT()
  r"\s":
    return IGNORE()

nimy testPar[MyToken]:
  top[string]:
    plus:
      return $1

  plus[string]:
    mult PLUS plus:
      return $1 & " + " & $3

    mult:
      return $1

  mult[string]:
    num MULTI mult:
      return "[" & $1 & " * " & $3 & "]"

    num:
      return $1

  num[string]:
    LPAREN plus RPAREN:
      return "(" & $2 & ")"

    ## float (integer part is 0-9) or integer
    NUM DOT[] NUM{}:
      result = ""
      # type of `($1).val` is `int`
      result &= $(($1).val)
      if ($2).len > 0:
        result &= "."
      # type of `$3` is `seq[MyToken]` and each elements are NUM
      for tkn in $3:
        # type of `tkn.val` is `int`
        result &= $(tkn.val)

test "test Lexer":
  var testLexer = testLex.newWithString("1 + 42 * 101010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var
    ret: seq[MyTokenKind] = @[]

  for token in testLexer.lexIter:
    ret.add(token.kind)

  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.MULTI,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM]

test "test Parser 1":
  var testLexer = testLex.newWithString("1 + 42 * 101010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var parser = testPar.newParser()
  check parser.parse(testLexer) == "1 + [42 * 101010]"

  testLexer.initWithString("1 + 42 * 1010")

  parser.init()
  check parser.parse(testLexer) == "1 + [42 * 1010]"

test "test Parser 2":
  var testLexer = testLex.newWithString("1 + 42 * 1.01010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var parser = testPar.newParser()
  check parser.parse(testLexer) == "1 + [42 * 1.01010]"

  testLexer.initWithString("1. + 4.2 * 101010")

  parser.init()
  check parser.parse(testLexer) == "1. + [4.2 * 101010]"

test "test Parser 3":
  var testLexer = testLex.newWithString("(1 + 42) * 1.01010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

  var parser = testPar.newParser()
  check parser.parse(testLexer) == "[(1 + 42) * 1.01010]"
```

Install
=======

1.  `nimble install https://github.com/yuanweixin/nimly`

Now, you can use nimly with `import nimly`.

