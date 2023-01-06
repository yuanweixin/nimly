import std/macros
{.experimental: "caseStmtMacros".}
import fusion/matching
import std/strutils

# Example lifted from 
# https://nim-lang.org/blog/2021/03/10/fusion-and-pattern-matching.html
# with the missing pieces of code from the project's test file
# https://github.com/nim-lang/fusion/blob/master/tests/tmatching.nim

# what I like: 
# * you can do `case blah:`, I always thought it's inconsistent that 
# you cannot put a colon at the end of case 
# * the pattern of NimNode -> IR -> NimNode makes perfect sense 
type
  FlowStageKind = enum
    fskMap # Stage for element conversion
    fskFilter # Filter elements
    fskEach # Execute action without returning value

  FlowStage = object
    outputType: Option[NimNode] # Assert result type
    kind: FlowStageKind # Type of the stage
    body: NimNode # Stage body

func makeTypeAssert(
      expType, body, it: NimNode): NimNode =
      let
        bodyLit = body.toStrLit().strVal().strip().newLit()
        pos = body.lineInfoObj()
        ln = newLit((filename: pos.filename, line: pos.line))

      return quote do:
        when not (`it` is `expType`):
          static:
            {.line: `ln`.}: # To get correct line number when `error`
                            # is used it is necessary to use
                            # `{.line.}` pragma.
              error "\n\nExpected type " & $(typeof(`expType`)) &
                ", but expression \e[4m" & `bodyLit` &
                "\e[24m has type of " & $(typeof(`it`))

func identToKind(id: NimNode): FlowStageKind =
      if id.eqIdent("map"):
        fskMap
      elif id.eqIdent("filter"):
        fskFilter
      elif id.eqIdent("each"):
        fskEach
      else:
        raiseAssert("#[ IMPLEMENT ]#")

proc rewrite(node: NimNode, idx: int): NimNode =
  case node:
    of Ident(strVal: "it"):
      result = ident("it" & $idx)
    of (kind: in nnkTokenKinds): # `nnkTokenKinds` is a set of node
                                 # kinds that don't have subnodes.
                                 # These ones are returned without any
                                 # modifications.
      result = node
    else:
      # For node kinds with subnodes, rewriting must be done
      # recursively
      result = newTree(node.kind)
      for subn in node:
        result.add subn.rewrite(idx)

func evalExprFromStages(stages: seq[FlowStage]): NimNode =
  result = newStmtList()
  for idx, stage in stages:
    # Rewrite body
    let body = stage.body.rewrite(idx)


    case stage.kind:
      # If stage is a filter it is converted into `if` expression
      # and new new variables are injected.
      of fskFilter:
        result.add quote do:
          let stageOk = ((`body`))
          if not stageOk:
            continue

      of fskEach:
        # `each` has no variables or special formatting - just
        # rewrite body and paste it back to resulting code
        result.add body
      of fskMap:
        # Create new identifier for injected node and assign
        # result of `body` to it.
        let itId = ident("it" & $(idx + 1))
        result.add quote do:
          let `itId` = `body`

        # If output type for stage needs to be explicitly checked
        # create type assertion.
        if Some(@expType) ?= stage.outputType:
          result.add makeTypeAssert(expType, stage.body, itId)

func typeExprFromStages(stages: seq[FlowStage], arg: NimNode): NimNode =
  let evalExpr = evalExprFromStages(stages)
  var
    resTuple = nnkPar.newTree(ident "it0")

  for idx, stage in stages:
    if stage.kind notin {fskFilter}:
      resTuple.add ident("it" & $(idx + 1))

  let lastId = newLit(stages.len - 1)

  result = quote do:
    block:
      (
        proc(): auto =
          for it0 {.inject.} in `arg`:
            `evalExpr`
            result = `resTuple`
      )()[`lastId`]

macro flow*(arg, body: untyped): untyped =
  var stages: seq[FlowStage]
  for elem in body:
    if elem.matches(
        Call[BracketExpr[@ident, opt @outType], @body] |
        # `map[string]:`
        Command[@ident is Ident(), Bracket [@outType], @body] |
        # `map [string]:`
        Call[@ident is Ident(), @body]
        # just `map:`, without type argument
      ):
        stages.add FlowStage(
          kind: identToKind(ident),
          outputType: outType,
          body: body
        )

  let evalExpr = evalExprFromStages(stages)

  if stages[^1].kind notin {fskEach}:
    # If last stage has return type (not `each`) then we need to
    # accumulate results in temporary variable.
    let resExpr = typeExprFromStages(stages, arg)
    let lastId = ident("it" & $stages.len)
    let resId = ident("res")
    result = quote do:
      var `resId`: seq[typeof(`resExpr`)]

      for it0 {.inject.} in `arg`:
        `evalExpr`
        `resId`.add `lastid`

      `resId`
  else:
    result = quote do:
      for it0 {.inject.} in `arg`:
        `evalExpr`


  result = newBlockStmt(result)