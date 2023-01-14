import unittest
import patty
import strutils
import macros
import std/math
import nimly
import std/options

## variant is defined in patty
variant MyToken:
  PLUS
  MULTI
  MINUS 
  DIV
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE
  EXPON

niml testLex[MyToken]:
  r"\(":
    return LPAREN()
  r"\)":
    return RPAREN()
  r"\+":
    return PLUS()
  r"-":
    return MINUS()
  r"\*":
    return MULTI()
  r"/":
    return DIV()
  r"\d+":
    return NUM(parseInt(token.token))
  r"^":
    return EXPON()
  r"\s":
    return IGNORE()

nimy testPar[MyToken, SLR]:
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

proc calculate(str: string) : Option[int] = 
  var
    lexer = testLex.newWithString($str)
  lexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var
     parser = testPar.newParser()
  return parser.parse_testPar(lexer)

test "nonassoc":
  check calculate("2^2^3") == none[int]()

test "top level prec rules":
    check calculate("20 + 1 * 2") == some 22
    check calculate("20+1*2+30") == some 52
    check calculate("1+2+3+4") == some 10 
    check calculate("1*2*3*4") == some 24
    check calculate("1*2+3*4") == some 14
    check calculate("-100 * -100") == some 10000