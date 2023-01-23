import patty
import nimyacc
import strutils
import common

variantp StateToken:
  SPLUS
  SMULTI
  SNUM(val: int)
  SID(str: string)
  SLPAREN
  SRPAREN
  SLBRACE
  SRBRACE
  SIF
  SELSE
  STHEN
  SIGNORE

genStringMatcher testStateLex[LexerState,StateToken]:
  r"\+":
    yield SPLUS()
  r"\*":
    yield SMULTI()
  r"\d+":
    yield SNUM(parseInt(input.substr(oldpos, pos-1)))
  r"if":
    yield SIF()
  r"else":
    yield SELSE()
  r"then":
    yield STHEN()
  r"\(":
    yield SLPAREN()
  r"\)":
    yield SRPAREN()
  r"\{":
    yield SLBRACE()
  r"\}":
    yield SRBRACE()
  r"[a-zA-Z_]\w*":
    yield SID(input.substr(oldpos, pos-1))
  r"\s":
    discard

nimy testStatePar[StateToken]:
  top[string]:
    state:
      return $1
  state[string]:
    SIF exp STHEN SLBRACE state SRBRACE SELSE SLBRACE state SRBRACE:
      return "IF(" & $2 & ")THEN{" & $5 & "}ELSE{" & $9 & "}"
    exp:
      return $1
  exp[string]:
    plus:
      return $1
  plus[string]:
    plus SPLUS plus:
      return $1 & "+" & $3
    mult:
      return $1
  mult[string]:
    mult SMULTI mult:
      return "(" & $1 & "*" & $3 & ")"
    num:
      return $1
  num[string]:
    SNUM:
      return $(($1).val)
    SID:
      return $(($1).str)
    SLPAREN exp SRPAREN:
      return "(" & $2 & ")"
