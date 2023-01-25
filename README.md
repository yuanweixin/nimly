# nimyacc

## LALR, SLR generator macro library in Nim. 

[![github\_workflow](https://github.com/yuanweixin/nimyacc/workflows/test/badge.svg)](https://github.com/yuanweixin/nimyacc/actions?query=workflow%3Atest)
[![nimble](https://raw.githubusercontent.com/yglukhov/nimble-tag/master/nimble.png)](https://github.com/yglukhov/nimble-tag)


The core algorithms (first/follow sets, canonical collection, lookahead propagation for lalr) is based on the code in [nimly](https://github.com/loloicci/nimly). Numerous performance enhancements and fixes were made on top of that code. 

## A list of features 
* Support for bison style %left, %right, %nonassoc declarations, as well as rule level %prec declarations to use in conflict resolution. 
* Output of debug string containing shift/conflict resolution info, the state machine itself, and the action and goto tables to the location specified by -d:nimydebug
* Output of dot string of automaton to the location specified by -d:nimygraphviz. 
* Error recovery using error symbol, similar to bison. This is the ad hoc error recovery using error token, as described in Modern Compiler Implementation in ML, p.76. I believe this is similar to what yacc/bison does. For another description of the algorithm, see the [python yacc clone PLY documentation](https://www.dabeaz.com/ply/ply.html#ply_nn29). 
* Use [lexim](https://github.com/yuanweixin/lexim) which is a high performance scanner library that implements the dfa directly as a jump table, eliminating the overhead of table lookups. It supports lexer states for ease of handling common constructs such as comments and strings.  

## Possible future extensions
* Add more grammars as examples.
* Local error recovery strategies that does not require any change to grammar. 
  
  Error rules are manual, invasive changes to the grammar and force the grammar writer to anticipate error sites. On the other hand, there's a whole class of error recovery techniques that rely on doing a bounded search of possible token insertions, deletions and change on the local parse context. For example, ml-yacc implements [burke fisher error repair](https://en.wikipedia.org/wiki/Burke%E2%80%93Fisher_error_repair). There's several other techniques. For a relatively recent list of references, see section 18.2.7 of [Parsing techniques: a practical guide](https://link.springer.com/book/10.1007/978-0-387-68954-8). It would be ideal for this library to provide at least 1 such automated error recovery option as an alternative to the error token. 

* Example-based error message specification

  The (state, input token, error message) data can be semi-automated as a series of (bad input, error message) that gets fed to the parser, and the state at which the parser error occur can be discovered during the parse. 

* Canonical LR with table compression

  This would give full LR(1) power with negligible overhead compared to LALR. This would also eliminate the class of "mysterious reduce/reduce" conflicts that can happen with LALR parsers. See [bison doc](https://www.gnu.org/software/bison/manual/html_node/Mysterious-Conflicts.html) for example of such conflicts. 

* Counterexample generation to debug grammar development 

  See [bison doc](https://www.gnu.org/software/bison/manual/html_node/Counterexamples.html) for a description of the feature.

## Installing

Add to .nimble file 
```
requires "https://github.com/yuanweixin/nimyacc >= 1.0.0"
```


## Usage Overview

The examples assume use of the [patty]() lib to create the case object types. 

```nim
variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE
```

Use of `patty` is optional. However, the parser generator does expect a  _naming convention_ for the "kind" parameter of the case object. 

Namely, if you case object is called `MyToken`, then the "kind" parameter must be `MyTokenKind`. In other words, it uses <TokName> & "Kind". 

## lexer usage 

You need to define an object type that have these fields: `startPos`, `endPosExcl`, like so: 
```nim
type LexerState* = object
    startPos*: int # needed for lexim  
    endPosExcl*: int # needed for lexim
```

Then you use the `genStringMatcher` macro to define your lexer. Note, it generates an iterator, so you must yield. You can also `discard` patterns you wish to ignore. 

```nim
genStringMatcher testLex[LexerState,MyToken]:
  r"\(":
    yield LPAREN()
  r"\)":
    yield RPAREN()
  r"\+":
    yield PLUS()
  r"\*":
    yield MULTI()
  r"\d":
    yield NUM(parseInt(input.substr(oldpos, pos-1)))
  r"\.":
    yield DOT()
  r"\s":
    discard
```
The above generates a `testLex` proc, that can be used to create a lexer like this: 
```nim
  var s: LexerState
  var lexer = testLex.newWithString(s, str)
```
You don't really need to call the lexer's procs directly. The typical usage is to just pass it to a parser later. But if you need to you can pull out the individual tokens. Note the example below is for the NimlLexer defined in [lexer.nim](src/nimyacc/lexer.nim), which uses the lexim api under the hood. 
```nim
  var s: LexerState
  var testLexer = testLex.newWithString(s, "some input string")
  while not testLexer.isEmpty():
    discard testLexer.lexNext()
```
`lexNext` returns a `LexerOutput` type which is a case object that can be of kind `Jammed`, `Token`, or `Eof` for lexical error, valid token, and end of file, respectively. 

If already at `Eof` or `Jammed`, future calls to `lexNext` will return `Eof` or `Jammed`, respectively. `isEmpty()` will return false. 

## parser usage 

macro `nimy` generates the parsing table and the code to invoke the parser, at compile time. The generated code simply creates a proc that, when called, invokes the parser engine on the given input, and use the parser table to derive the parse tree. The parse tree is then traversed post-order. During the traversal, the user-specified code blocks are run. 

In an actual parser, you'd need to pass symbol table objects. In the examples below we will use a dummy one: 
```nim
type UserActionState = object
  dummy: int
```
This is exposed to the user code blocks as `uastate`. 

The dsl looks like this: 
```nim
nimy testPar[MyToken, UserActionState]: # generates parse_testPar proc. 
  top[string]: # left hand side "top", returning a string. 
    NUM: # right hand side "plus"
      return $1 # user action. supports the $n references to the matches. n>=1. 
```
In the above, `top` is the start symbol. It is always the first nonterminal that appears in the specification. 

Also, note the `$n` syntax for retrieving the subtrees. n>=1. Note that this is implemented by pattern matching the nim ast for `Prefix([Ident(strVal: "$"), IntLit()])`, so $n usually needs to be surrounded by spaces or ($n), otherwise nim might confuse it as part of some other syntax. 

The above example generats a `testPar` constant, which is of type `ParsingTable`. It can be used to construct a new parser using `newParser`. 

The above example generates a `parse_testPar` procedure. It always adds the `parse_` prefix to the name (in this case `testPar`). The type is: 
```
proc parse_testPar[LexerState, UserActionState, Token](p: var Parser, l: NimlLexer[LexerState,Token], uas: var UserActionState) : Option[Token]
```

Snippet of using the generated `parse_testPar` routine in the example: 
```nim
  var s: LexerState
  var uas: UserActionState
  var 
    lexer = testLex.newWithString(s, str)
    parser = testPar.newParser()
  return parser.parse_testPar(lexer, uas)
```
If there is a parse error, then `parser.hasError` returns true. 

Note that different left hand sides can have different return types. 
```nim
nimy testPar[MyToken, UserActionState]:
  top[string]:
    plus:
      return $($1) # plus returns an int, so we need to convert it to string

  plus[int]:
    NUM:
      return ($1).val # extract the int value of the NUM token.
```

Option: 0 or 1 matches:

The type is `seq[xxx]` where `xxx` is type of `XXX`.
```nim
nimy testOption[MyToken, UserActionState]:
  num[int]:
    DOT{}:
      var cnt = 0
      for d in $1:
        inc cnt
      return cnt
```

Repetition: 0 or more matches:

The type is `seq[xxx]` where `xxx` is type of `XXX`.
```nim
nimy testRep[MyToken, UserActionState]:
  num[int]:
    DOT{}:
      var cnt = 0
      for d in $1:
        inc cnt
      return cnt
```
Specifying precedence of tokens. Note using a fake token to override rule level precendence is also supported (UMINUS in this example).
```nim
nimy testPar[MyToken, UserActionState, SLR]:
  %left PLUS MINUS
  %left MULTI DIV
  %nonassoc EXPON
  %nonassoc UMINUS

  exp[int]:
    NUM:
      return ($1).val
    exp PLUS exp:
      return $1 + $3
    exp MINUS exp:
      return $1 - $3
    exp MULTI exp:
      return $1 * $3
    exp DIV exp:
      return $1 div $3
    exp EXPON exp:
      return int(math.pow(float64($1), float64($3)))
    MINUS exp %prec UMINUS:
      return -($2)
```
Here's an example of the use of the error symbol for error recovery. See [test case](tests/test_error_symbol.nim). 
```nim
nimy testPar[MyToken, UserActionState]:
  exps[int]:
    exp:
      return 1 
    exps exp:
      return 1 
    error exp:
      return 1 
    
  exp[int]:
    ID: 
      return 1 
    exp PLUS exp:
      return 1 
    LPAREN exps RPAREN:
      return 1 
    LPAREN error RPAREN: 
      return 1 
```

## A complete example [test case](tests/test_readme_example.nim)

```nim
import unittest
import patty
import strutils
import options
import nimyacc
import common

## variant is defined in patty
variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE

genStringMatcher testLex[LexerState,MyToken]:
  r"\(":
    yield LPAREN()
  r"\)":
    yield RPAREN()
  r"\+":
    yield PLUS()
  r"\*":
    yield MULTI()
  r"\d":
    yield NUM(parseInt(input.substr(oldpos, pos-1)))
  r"\.":
    yield DOT()
  r"\s":
    discard

nimy testPar[MyToken, UserActionState]:
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
  var s: LexerState
  var testLexer = testLex.newWithString(s, "1 + 42 * 101010")
  var
    ret: seq[MyTokenKind] = @[]

  while not testLexer.isEmpty():
    ret.add testLexer.lexNext().token.kind
    
  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.MULTI,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM]

test "test Parser 1":
  var s: LexerState
  var uas: UserActionState
  var testLexer = testLex.newWithString(s, "1 + 42 * 101010")

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer, uas) == some "1 + [42 * 101010]"

  testLexer = testLex.newWithString(s, "1 + 42 * 1010")

  parser.init()
  check parser.parse_testPar(testLexer, uas) == some "1 + [42 * 1010]"

test "test Parser 2":
  var s: LexerState
  var uas: UserActionState
  var testLexer = testLex.newWithString(s, "1 + 42 * 1.01010")

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer, uas) == some "1 + [42 * 1.01010]"

  testLexer = testLex.newWithString(s, "1. + 4.2 * 101010")

  parser.init()
  check parser.parse_testPar(testLexer, uas) == some "1. + [4.2 * 101010]"

test "test Parser 3":
  var s: LexerState
  var uas: UserActionState
  var testLexer = testLex.newWithString(s, "(1 + 42) * 1.01010")

  var parser = testPar.newParser()
  check parser.parse_testPar(testLexer, uas) == some "[(1 + 42) * 1.01010]"
```

## Development

Due to the limitations on loop iterations in the nim compiler vm (see maxLoopIterationsVM flag in the [nim compiler user guide](https://nim-lang.org/docs/nimc.html) for more info), parser table generation is offloaded to a separate executable `yexe` (otherwise the vm runs out of iterations and kills itself, plus it runs orders of magnitude slower than if logic is in a separate executable). `staticExec` to yexe is done in the `nimy` macro. If you make changes to `yexe` you need to run `dev_nimble_install.sh` to update the local version. 

The json parsing of debug/dot string is apparently too expensive for the nim vm, so `yexe` is also responsible for outputting to the file paths in `-d:nimydebug=<debug_path>` and `-d:nimygraphviz=<dot_path>`. It does return error to the caller of `staticExec` so error message surfaces as an unhandled exception during compile time. See `parsegen.nim` for details. 

If you are not modifying yexe, then it should already be installed for you when you install nimyacc, and should not cause issues. 

[parsetypes.nim](src/nimyacc/parsetypes.nim) contains most of the type definitions, as well as the logic to compute FIRST and FOLLOW. 

[slr.nim](src/nimyacc/slr.nim) contains logic to compute the canonical collection. 

[lalr.nim](src/nimyacc/lalr.nim) contains logic to compute the lookahead propagation. 

[parsegen.nim](src/nimyacc/parsegen.nim) contains the logic to handle the dsl, call yexe, and do codegen. When changing this logic, you can use -dnimydebug to trigger the printing of the generated code to iterate. Note that due to use of a generated macro, the printed code isn't the final expanded version. But if you want you can wrap the `nimy` macro in the `expandMacro` call and see the actual finally generated code (most of the lines should contain the ParsingTable object). 

[debuginfo.nim](src/nimyacc/debuginfo.nim) contains logic to generate a debug or dot string from the parser automaton. 

[parser.nim](src/nimyacc/parser.nim) contains the parser engine loop. The error recovery mechanism is also in there. 