import parsetypes, debuginfo, slr, lalr, std/jsonutils, json

type YexeInput* = object
  g*: Grammar 
  parserType*: ParserType 
  dctx*: DebugContext

type YexeOutput* = object
  pt*: ParsingTable
  dctx*: DebugContext

proc main(input:string) : string = 
  var i: YexeInput
  fromJson(i, parseJson(input))
  var o: YexeOutput
  o.pt = 
    case i.parserType 
    of Slr:
      makeTableSLR(i.g, i.dctx)
    of Lalr:
      makeTableLALR(i.g, i.dctx)
  o.dctx = i.dctx
  echo $o.toJson()

when isMainModule:
  echo main(readAll(stdin))
