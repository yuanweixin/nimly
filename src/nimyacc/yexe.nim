import parsetypes, slr, lalr, std/jsonutils, json


proc main(input: string): string =
  var g: Grammar
  fromJson(g, parseJson(input))
  let pt = 
    case g.parserType 
    of Slr:
      makeTableSLR(g)
    of Lalr:
      makeTableLALR(g)  
  result = $pt.toJson()

echo main(readAll(stdin))
