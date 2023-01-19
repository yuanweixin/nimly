type
  TokIter[LS,T] = iterator(lexState: var LS): T {.closure.} 
  NimlLexer*[LS,T] = object  # LS: lexer state type, T: token type
    tokStream*: TokIter[LS,T]
    lastRead: T
    lexState: LS

  NimlError* = object of Exception
  NimlEOFError* = object of NimlError

proc newWithString*[LS,T](makeLex: proc (s: string) : TokIter[LS,T], lexState: LS, str: string): NimlLexer[LS,T] =
  result.lexState = lexState
  result.tokStream = makeLex(str)
  result.lastRead = result.tokStream(result.lexState)

proc isEmpty*[LS,T](nl: NimlLexer[LS,T]): bool =
  return finished(nl.tokStream)

proc lexNext*[LS,T](nl: var NimlLexer[LS,T]): T =
  if not finished(nl.tokStream):
    let retVal = nl.lastRead
    nl.lastRead = nl.tokStream(nl.lexState)
    return retVal 
  raise newException(NimlEOFError, "read EOF")

iterator lexIter*[LS,T](nl: var NimlLexer[LS,T]): T =
  while not finished(nl.tokStream):
    yield nl.lastRead 
    nl.lastRead = nl.tokStream(nl.lexState)
