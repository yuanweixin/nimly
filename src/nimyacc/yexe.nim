import parsetypes, debuginfo, slr, lalr, std/jsonutils, json

type YexeInput* = object
  g*: Grammar 
  parserType*: ParserType 
  doGenDebugString* : bool
  debugPath* : string 
  dotPath* : string
  doGenGraphViz* : bool  

type YexeOutput* = object
  pt*: ParsingTable 
  hasError*: bool
  errMsg*: string 

proc main(input:string) : string = 
  var i: YexeInput
  var res : YexeOutput 
  try:
    fromJson(i, parseJson(input))
    var dctx : DebugContext
    dctx.doGenDebugString = i.doGenDebugString
    dctx.doGenGraphViz = i.doGenGraphViz
    res.pt = 
      case i.parserType 
      of Slr:
        makeTableSLR(i.g, dctx)
      of Lalr:
        makeTableLALR(i.g, dctx)
    if i.doGenDebugString:
      writeFile(i.debugPath, dctx.debugStr)
    if i.doGenGraphViz:
      writeFile(i.dotPath, dctx.dotStr)
  except Exception as e:
    res.hasError = true
    res.errMsg = e.msg
  echo $res.toJson()

when isMainModule:
  echo main(readAll(stdin))
