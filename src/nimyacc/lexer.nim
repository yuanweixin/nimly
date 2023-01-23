from lexim import JammedException

type
  TokIter[LS,T] = iterator(lexState: var LS): T {.closure.} 
  NimlLexer*[LS,T] = object  # LS: lexer state type, T: token type
    tokStream*: TokIter[LS,T]
    lastRead: LexerOutput[T]
    lexState: LS
    input*: string
    isJammed* : bool

  LexerOutputKind* = enum
    Token
    Eof
    Jammed
  LexerOutput*[T] = object
    case kind* : LexerOutputKind
    of Token:
      token* : T
      startPos* : int
      endPosExcl* : int 
    else: 
      discard 

proc `==`*[T](a,b: LexerOutput[T]) : bool = 
  if a.kind == b.kind:
    case a.kind 
    of Token:
      return a.token == b.token and a.startPos == b.startPos and a.endPosExcl == b.endPosExcl
    else:
      return true 
  return false

proc readHelper[LS,T](nl: var NimlLexer[LS,T]) : LexerOutput[T] = 
  try:
    let read = nl.tokStream(nl.lexState)
    if finished(nl.tokStream):
      result = LexerOutput[T](kind: Eof)
    else:
      result = LexerOutput[T](kind: Token, token: read, startPos: nl.lexState.startPos, endPosExcl: nl.lexState.endPosExcl)
  except JammedException:
    nl.isJammed = true
    result = LexerOutput[T](kind: Jammed)

proc newWithString*[LS,T](makeLex: proc (s: string) : TokIter[LS,T], lexState: LS, str: string): NimlLexer[LS,T] =
  result.lexState = lexState
  result.tokStream = makeLex(str)
  result.lastRead = result.readHelper()
  result.input = str

proc isEmpty*[LS,T](nl: NimlLexer[LS,T]): bool =
  ## note: this returns true if finished or lexer is jammed.
  return finished(nl.tokStream) or nl.isJammed

proc lexNext*[LS,T](nl: var NimlLexer[LS,T]): LexerOutput[T] =
  ## returns the next token. 
  ## if we are out of tokens, will return Eof. 
  ## if we are jammed, will return Jammed. 
  let last = nl.lastRead 
  nl.lastRead = nl.readHelper()
  return last 
