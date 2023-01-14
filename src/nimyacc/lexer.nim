type
  # EOF but didn't accept last few chunks of input
  # Not EOF but didn't accept last few chunks of input 
  # Old lexer does throw for those conditions. 
  # Lexim currently throws a generic Exception when encountering jam state. 
  # TODO revise it to something else
  # tldr: this isn't used by lexim but keeping here as reminder to fix exception behavior in lexer
  LexError* = object of Exception

type
  TokIter[LS,T] = iterator(lexState: var LS): T {.closure.} 
  NimlLexer*[LS,T] = object  # LS: lexer state type, T: token type
    tokStream*: TokIter[LS,T]
    lastRead: T
    lexState: LS

  # used as a base type for eof error, TODO eliminate me. 
  NimlError* = object of Exception
  # used in parser, so for now will keep throwing it
  # TODO eliminate after successful lexim integration. 
  NimlEOFError* = object of NimlError

# TODO this is used in many test case 
proc newWithString*[LS,T](makeLex: proc (s: string) : TokIter[LS,T], lexState: LS, str: string): NimlLexer[LS,T] =
  result.lexState = lexState
  result.tokStream = makeLex(str)
  result.lastRead = result.tokStream(result.lexState)

# TODO used in a few tests but totally unnecessary in lexim
proc close*[LS,T](lexer: var NimlLexer[LS,T]) =
  discard

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
