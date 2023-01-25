import patty
import nimyacc
import common

variantp MyTerm:
  EQ
  ST
  ID(val: string)
  IGNORE

genStringMatcher lex415[LexerState,MyTerm]:
  r"=":
    yield EQ()
  r"\*":
    yield ST()
  r"[a-zA-Z\-_][a-zA-Z0-9\-_]*":
    yield ID(input.substr(oldpos, pos-1))
  r"\s":
    discard

nimy psr415LALR[MyTerm, UserActionState]:
  start[string]:
    left EQ right:
      return $1 & "=" & $3
    right:
      return $1
  left[string]:
    ST right:
      return "*" & $2
    ID:
      return ($1).val
  right[string]:
    left:
      return $1
