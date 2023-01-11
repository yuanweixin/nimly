{.experimental: "caseStmtMacros".}
import macros
import tables
import sets

import parsetypes
import parser
import fusion/matching

type
  PTProc[T, S, R] = proc(nimlytree: ParseTree[T, S]): R {.nimcall.}
  RuleToProc*[T, S, R] = Table[Rule[S], PTProc[T, S, R]]
  NimyKind = enum
    NonTerm
    Term
  NimyRow = object
    kind: NimyKind
    retTyNode: NimNode
    ruleToProc: NimNode
    optRule: NimNode
    repRule: NimNode
  NimyInfo = Table[string, NimyRow]
  ParserType = enum
    Slr
    Lalr 

iterator iter(topClause, body: NimNode, optAndRep: seq[NimNode]): (int, NimNode) =
  var cnt = 0
  yield (cnt, topClause)
  inc(cnt)
  for val in body:
    yield (cnt, val)
    inc(cnt)
  for val in optAndRep:
    yield (cnt, val)
    inc(cnt)

proc initNimyRow(kind: NimyKind,
                 rtn: NimNode = newEmptyNode(),
                 rtp: NimNode = newEmptyNode(),
                 opr: NimNode = newEmptyNode(),
                 rpr: NimNode = newEmptyNode()): NimyRow =
  result = NimyRow(kind: kind, retTyNode: rtn, ruleToProc: rtp, optRule: opr,
                   repRule: rpr)

proc isNonTerm(s: string, nimyInfo: NimyInfo): bool =
  if not nimyInfo.haskey(s):
    return false
  return nimyInfo[s].kind == NonTerm

proc isTerm(s: string, nimyInfo: NimyInfo): bool =
  if not nimyInfo.haskey(s):
    return false
  return  nimyInfo[s].kind == Term

proc initNimyInfo(): NimyInfo =
  return initTable[string, NimyRow]()

proc initRuleToProc*[T, S, R](): RuleToProc[T, S, R] =
  return initTable[Rule[S], PTProc[T, S, R]]()

proc initRuleToProcNode(tokenType, tokenKind, returnType: NimNode): NimNode =
  result = quote do:
    result = initRuleToProc[`tokenType`, `tokenKind`, `returnType`]()

proc genKindNode(kindTy, kind: NimNode): NimNode =
  result = nnkDotExpr.newTree(
    kindTy,
    kind
  )

proc failwith(reason: string) = 
  raise newException(Exception, reason)

proc failwith(node: NimNode) = 
  raise newException(Exception, "I do not understand this: " & repr node)

proc convertToSymNode(name: string, kindTy: NimNode,
                      nimyInfo: NimyInfo): NimNode =
  if name.isNonTerm(nimyInfo):
    let nameStrlit = newStrLitNode(name)
    result = quote do:
      NonTermS[`kindTy`](`nameStrLit`)
  elif name.isTerm(nimyInfo):
    if name == "error":
      result = quote do:
        ErrorS[`kindTy`]()
    else:
      let nameId = ident(name)
      result = quote do:
        TermS[`kindTy`](`kindTy`.`nameId`)
  else:
    doAssert false

proc convertToSymNode(node, kindTy: NimNode,
                      nimyInfo: NimyInfo,
                      noEmpty: bool = true): NimNode =
  node.expectKind({nnkIdent, nnkBracket, nnkBracketExpr, nnkCurlyExpr})
  case node
  of BracketExpr([@innerSym]):
    return nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("NonTermS"),
        kindTy
      ),
      newStrLitNode(nimyInfo[innerSym.strVal].optRule.strVal)
    )
  of CurlyExpr([@innerSym]):
    return nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("NonTermS"),
        kindTy
      ),
      newStrLitNode(nimyInfo[innerSym.strVal].repRule.strVal)
    )
  of Bracket([]):
    return nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("Empty"),
        kindTy
      )
    )
  of Ident(strVal: @name):
    return convertToSymNode(name, kindTy, nimyInfo)
  else:
    failwith node

proc newRuleMakerNode(kindTy, prec: NimNode, left: NimNode,
                      right: varargs[NimNode]): NimNode =
  result = nnkCall.newTree(
    nnkBracketExpr.newTree(
      newIdentNode("newRule"),
      kindTy
    ),
    prec,
    left
  )
  for node in right:
    result.add(node)

proc nonTermOrEmpty(node: NimNode, nimyInfo: NimyInfo): string =
  case node
  of Bracket():
    return ""
  of BracketExpr([(strVal: @strVal)]):
    return nimyInfo[strVal].optRule.strVal
  of CurlyExpr([(strVal: @strVal)]):
    return nimyInfo[strVal].repRule.strVal
  else:
    let s = node.strVal
    if s.isNonTerm(nimyInfo):
      result = s
    else:
      result = ""

proc isTerm(node: NimNode, nimyInfo: NimyInfo): bool =
  case node
  of Bracket() | BracketExpr() | CurlyExpr():
    return false 
  of Ident(strVal: @strVal):
    return not strVal.isNonTerm(nimyInfo)
  else:
    failwith "unexpected rhs symbol " & repr node
  
iterator ruleRight(node: NimNode, excludePrec : bool = true): NimNode =
  # this ends up yielding each NimNode on the rhs of production
  case node
  of Call([@fst,.._]):
    yield fst
  of Command():
    var nd = node
    while nd.kind == nnkCommand:
      yield nd[0]
      nd = nd[1]
    if excludePrec:
      if nd.kind != nnkPrefix: # e.g. %prec Token
        yield nd 
    else:
      yield nd 
  else:
    failwith "I do not understand this rule rhs: " & repr node

proc parseRuleAndBody(node, kindTy, tokenType, left: NimNode,
                      nimyInfo: var NimyInfo, precAssoc: var Table[string,(Precedence,Associativity)]): (
                        NimNode, seq[string], NimNode, Option[Precedence]) =
  node.expectKind({nnkCall, nnkCommand})
  var
    right: seq[NimNode] = @[]
    types: seq[string] = @[]
    body: NimNode
    noEmpty: bool

  case node
  of Call([_, @b, .._]):
    body = b 
    noEmpty = false 
  of Command([_,_,@b, .._]):
    body = b 
    noEmpty = true 
  else:
    failwith "Unable to extract body from " & repr node

  var prec = none[Precedence]()
  for sym in node.ruleRight(excludePrec=false):
    if sym.kind == nnkPrefix:
      if sym.matches(Prefix([_, Command([_, Ident(strVal: @tok)])])):
        if tok notin precAssoc:
          failwith "missing top level precedence declaration for token " & tok & " used in " & repr node
        prec = some(precAssoc[tok][0])
      else:
        failwith "bug in dsl validation"
    else:
      right.add(sym.convertToSymNode(kindTy, nimyInfo, noEmpty))
      types.add(sym.nonTermOrEmpty(nimyInfo))
  let precNode = 
    if prec.isNone:
      quote do:
        none[Precedence]()
    else:
      let pv = prec.get
      quote do:
        some[Precedence](`pv`)
  let ruleMaker = newRuleMakerNode(kindTy, precNode, left, right)
  result = (ruleMaker, types, body, prec)

proc parseLeft(clause: NimNode): (string, NimNode) =
  case clause
  of Call([BracketExpr([(strVal: @nonTerm), @rType]), .._]):
    return (nonTerm, rType)
  else:
    failwith "lhs is not in expected form, got " & repr clause
  
proc isSpecialVar(n: NimNode): bool =
  return n.matches(Prefix([Ident(strVal: "$"), IntLit()]))

proc replaceBody(body, param: NimNode,
                 types: seq[string], nimyInfo: NimyInfo): NimNode =
  proc replaceImpl(body: NimNode): NimNode =
    if body.isSpecialVar:
      let index = int((body[1].intVal) - 1)
      # term
      if types[index] == "":
        result = quote do:
          `param`.tree[`index`].token
      # nonterm
      else:
        let ruleToProc = nimyInfo[types[index]].ruleToProc
        result = quote do:
          `ruleToProc`[`param`.tree[`index`].rule](`param`.tree[`index`])
    else:
      if body.len > 0:
        result = newTree(body.kind)
        for c in body:
          result.add(c.replaceImpl)
      else:
        result = body
  result = replaceImpl(body)

proc makeRuleProc(name, body, rTy, tokenType, tokenKind: NimNode,
                  types: seq[string], nimyInfo: NimyInfo, pt=false): NimNode =
  let
    param = newIdentNode("nimlytree")
    pTy =   nnkBracketExpr.newTree(newIdentNode("ParseTree"),
                                   tokenType, tokenKind)
    params = @[rTy, nnkIdentDefs.newTree(param, pTy, newEmptyNode())]
  var
    procBody: NimNode
  if not pt:
    procBody = body.replaceBody(param, types, nimyInfo)
    result = newProc(name, params, procBody)
  else:
    result = newProc(name, params)

proc tableMakerProc(name, tokenType, tokenKind, topNonTerm,
                    tableMaker: NimNode,
                    rules, syms: seq[NimNode], precedence: var Table[string,(Precedence, Associativity)]): NimNode = 
  var body = nnkStmtList.newTree()
  body.add quote do:
    when defined(nimydebug):
      echo "START: making the Parser"
  let
    setId = genSym(nskVar)
    grmId = genSym(nskVar)
  body.add quote do:
    var `setId`: seq[Rule[`tokenKind`]] = @[]
  
  for rule in rules:
    body.add quote do:
      `setId`.add(`rule`)
  
  body.add quote do:
    var `grmId` = initGrammar(`setId`, `topNonTerm`)
  
  if precedence.len > 0:
    for tok, (prec, assoc) in precedence:
      body.add quote do:
        `grmId`.precAssoc[`tok`]= (`prec`, Associativity(`assoc`))
  body.add quote do:
    result = `tableMaker`[`tokenKind`](`grmId`)
  result = quote do:
    proc `name`(): ParsingTable[`tokenKind`] =
      `body`

proc getOpt(sym, ty, nt: NimNode): NimNode =
  # for input:
  # sym=DOT, ty=MyToken, nt=__opt_DOT
  # 
  # generates: 
  # __opt_DOT[seq[MyToken]] do:
  #     DOT:
  #       return @[$1]
  #     []:
  #       return @[]
  # can't quote do this because it generates some open symbols 
  # which breaks the hacky pattern matching this dsl uses elsewhere
  # can't genAst because it does not generate the Call(Bracket(...)) structure
  # this code expects elsewhere...
  result = nnkCall.newTree(
    nnkBracketExpr.newTree(
      nt,
      nnkBracketExpr.newTree(
        newIdentNode("seq"),
        ty
      )
    ),
    nnkStmtList.newTree(
      nnkCall.newTree(
        sym,
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
                nnkPrefix.newTree(
                  newIdentNode("$"),
                  newLit(1)
                )
              )
            )
          )
        )
      ),
      nnkCall.newTree(
        nnkBracket.newTree(
        ),
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
              )
            )
          )
        )
      )
    )
  )

proc getRepOpt(sym, ty, nt: NimNode): NimNode =
  # for input:
  # sym=NUM, ty=MyToken, nt=__rep_NUM, nnt=__inner_rep_NUM
  # 
  # produces:
  # __rep_NUM[seq[MyToken]] do:
  #   __inner___rep_NUM:
  #     return $1
  #   []:
  #     return @[]
  # can't quote do this because it generates some open symbols 
  # which breaks the hacky pattern matching this dsl uses elsewhere
  # can't genAst because it does not generate the Call(Bracket(...)) structure
  # this code expects elsewhere...
  result = nnkCall.newTree(
    nnkBracketExpr.newTree(
      nt,
      nnkBracketExpr.newTree(
        newIdentNode("seq"),
        ty
      )
    ),
    nnkStmtList.newTree(
      nnkCall.newTree(
        sym,
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(1)
            )
          )
        )
      ),
      nnkCall.newTree(
        nnkBracket.newTree(
        ),
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
              )
            )
          )
        )
      )
    )
  )

proc getRep(sym, ty, nt, nnt: NimNode): seq[NimNode] =
  # e.g. sym=NUM, ty=MyToken, nt=__rep_NUM, nnt=__inner_rep_NUM
  # below produces: 
  # __inner___rep_NUM[seq[MyToken]] do:
  #   __inner___rep_NUM NUM:
  #       result = $1
  #       result.add($2)
  #   NUM:
  #     return @[$1]
  # can't quote do this because it generates some open symbols 
  # which breaks the hacky pattern matching this dsl uses elsewhere
  # can't genAst because it does not generate the Call(Bracket(...)) structure
  # this code expects elsewhere...
  result = @[]
  result.add(getRepOpt(nnt, ty, nt))
  let new = nnkCall.newTree(
    nnkBracketExpr.newTree(
      nnt,
      nnkBracketExpr.newTree(
        newIdentNode("seq"),
        ty
      )
    ),
    nnkStmtList.newTree(
      nnkCommand.newTree(
        nnt,
        sym,
        nnkStmtList.newTree(
          nnkAsgn.newTree(
            newIdentNode("result"),
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(1)
            )
          ),
          nnkCall.newTree(
            nnkDotExpr.newTree(
              newIdentNode("result"),
              newIdentNode("add")
            ),
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(2)
            )
          )
        )
      ),
      nnkCall.newTree(
        sym,
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
                nnkPrefix.newTree(
                  newIdentNode("$"),
                  newLit(1)
                )
              )
            )
          )
        )
      )
    )
  )
  result.add(new)

func parseHead(head: NimNode) : (NimNode, NimNode, ParserType) = 
  if head.matches(
    BracketExpr([@parserName, @tokenType, (strVal : @parserType)]) | 
    BracketExpr([@parserName, @tokenType])):
    if parserType.get("LALR") == "LALR":
      return (parserName, tokenType, Lalr)
    if parserType.get == "SLR":
      return (parserName, tokenType, Slr)
    failwith "I only understand {LALR, SLR} but got unsupport parser type " & parserType.get
  else:
    failwith "I expected nimy <parserName>[<tokType>,[<parserType>]]"

func validRhsSymType(n: NimNode) : bool = 
  return n.matches(Ident() | BracketExpr([Ident()]) | CurlyExpr([Ident()]))

func validRuleLevelPrec(n: NimNode) : bool = 
  # %prec <SomeString>
  return n.matches(Prefix([Ident(strVal: "%"), 
              Command([Ident(strVal: "prec"), Ident()])]))

func validAssociativity(n: NimNode) : bool = 
  return n.matches(Ident(strVal: in ["left", "right", "nonassoc"]))

proc validateRuleBody(n: NimNode) = 
  case n
  of CommentStmt():
    discard 
  of Call([Bracket(), StmtList()]) | Call([_.validRhsSymType(), StmtList()]): 
    discard 
  of Command([_.validRhsSymType(), _.validRhsSymType(), StmtList()]):
    discard
  of Command([_.validRhsSymType(), _.validRuleLevelPrec(), StmtList()]):
    discard
  of Command([_.validRhsSymType(), @c is Command(), StmtList()]):
    while c.kind == nnkCommand:
      if not c[0].validRhsSymType():
        failwith "invalid rule body "  & repr n 
      c = c[1]
    if not c.validRhsSymType() and not c.validRuleLevelPrec():
      failwith "invalid rule body " & repr n
  else:
    failwith "invalid rule body " & repr n 

func validNestedTypeBracketExpr(n: NimNode) : bool = 
  var nd = n 
  while nd.kind == nnkBracketExpr:
    if nd[0].kind != nnkIdent:
      return false 
    nd = nd[1]
  return nd.kind == nnkIdent

func validToken(n: NimNode) : bool = 
  return n.matches(Ident(strVal: != "error"))

proc validateRule(n : NimNode) = 
  case n 
  of Prefix([Ident(strVal: "%"), Command([_.validAssociativity(), _.validToken()])]):
    discard
  of Prefix([Ident(strVal: "%"), Command([_.validAssociativity(), @rest is Command()])]):
    while rest.kind == nnkCommand:
      if not rest[0].validToken():
        failwith "invalid associativity declaration " & repr n 
      rest = rest[1]
    if not rest.validToken():
      failwith "invalid associativity declaration " & repr n 
  of Call([BracketExpr([Ident(strVal: @lhs), @idOrNestedType is Ident()|BracketExpr()]), @rest is StmtList()]): # top[string] vs top[seq[string]]
    if lhs == "error":
      failwith "'error' is reserved for error symbol, cannot use it as a nonterminal, but got " & repr n
    if idOrNestedType.kind == nnkBracketExpr and not idOrNestedType.validNestedTypeBracketExpr():
      failwith "Invalid return type declaration in: " & repr n
    for ruleBody in rest:
      ruleBody.validateRuleBody()
  of CommentStmt():
    discard
  of Prefix():
    failwith "invalid associativity declaration " & repr n 
  else:
    failwith "invalid rule : " & repr n

proc validateBody(n : NimNode) = 
  doAssert n.kind == nnkStmtList 
  for group in n:
    group.validateRule()

proc getAssociativity(n: NimNode): Associativity = 
  case n 
  of Prefix([_,Command([(strVal: @assoc), _])]):
    if assoc == "left":
      return Left
    elif assoc == "right":
      return Right
    elif assoc == "nonassoc":
      return NonAssoc
    doAssert false, "bug in dsl validation"
  else:
    doAssert false, "bug in dsl validation"

iterator precAssocToks(n: NimNode): string = 
  case n 
  of Prefix([_, Command([_, Ident(strVal: @tok)])]):
    yield tok 
  of Prefix([_, Command([_, @rest is Command()])]):
    while rest.kind == nnkCommand:
      yield rest[0].strVal
      rest = rest[1]
    yield rest.strVal

macro nimy*(head, body: untyped): untyped =
  let 
    (parserName, tokenType, parserType) = parseHead(head)
    tokenKind = ident(tokenType.strVal & "Kind")
    tableMaker = case parserType
        of Slr:
          ident("makeTableLR")
        of Lalr:
          ident("makeTableLALR")
  body.validateBody()
  
  var
    nimyInfo = initNimyInfo()
    first = true
    topNonTerm: string
    topNonTermNode: NimNode
    returnType: Nimnode
    ruleIds: seq[NimNode] = @[]
    ruleDefs: seq[NimNode] = @[]
    ruleProcs: seq[NimNode] = @[]
    ruleToProcMakers: seq[NimNode] = @[]
    tableConstDefs: seq[NimNode] = @[]
    ruleProcPts: seq[NimNode] = @[]
    symNodes: seq[NimNode] = @[]
    curPrecedence = 0 
    precedence: Table[string,(Precedence, Associativity)]
  let topProcId = genSym(nskProc)
  result = newTree(nnkStmtList)

  # generate the precedence table
  for clause in body:
    if clause.kind != nnkPrefix:
      continue 
    let assoc = clause.getAssociativity
    for tok in clause.precAssocToks:
      precedence[tok] = (curPrecedence, assoc)
    inc curPrecedence

  # read BNF first (collect info)
  for clause in body:
    if clause.kind notin {nnkCall, nnkCommand}:
      continue
    let (nonTerm, rType) = parseLeft(clause)
    doAssert (not (nimyInfo.haskey(nonTerm))), nonTerm & " appears more than once in the spec as the lhs of a rule"
    nimyInfo[nonTerm] = initNimyRow(NonTerm, rtn = rType,
                                    rtp = genSym(nskConst))
    if first:
      topNonTerm = nonTerm
      topNonTermNode = nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("NonTermS"),
          tokenKind
        ),
        newStrLitNode(nonTerm)
      )
      returnType = rType
      first = false
  nimyInfo["__Start__"] = initNimyRow(NonTerm,
                                      rtn = returnType,
                                      rtp = genSym(nskConst))

  # make opt and rep
  var optAndRep: seq[NimNode] = @[]
  for clause in body:
    if clause.kind notin {nnkCall, nnkCommand}:
      continue
    for ruleClause in clause[1]:
      if ruleClause.kind == nnkCommentStmt:
        continue
      for sym in ruleClause.ruleRight:
        if sym.isTerm(nimyInfo) and not(nimyInfo.haskey(sym.strVal)):
          nimyInfo[sym.strVal] = initNimyRow(Term)
        if not (sym.kind in {nnkBracketExpr, nnkCurlyExpr}):
          continue # not opt or rep 
        doAssert sym.len == 1
        let innerSym = sym[0].strVal
        if sym[0].isTerm(nimyInfo) and
           not(nimyInfo.haskey(innersym)):
          nimyInfo[innerSym] = initNimyRow(Term)
        case sym.kind
        of nnkBracketExpr:
          if nimyInfo[innerSym].optRule.kind != nnkEmpty:
            continue
          let
            newStr = "__opt_" & innerSym
            new = newIdentNode(newStr)
            ty = if innerSym.isNonTerm(nimyInfo):
                   nimyInfo[innerSym].retTyNode
                 else:
                   tokenType
            rt = nnkBracketExpr.newTree(
              newIdentNode("seq"),
              ty
            )
            nr = nimyInfo[innerSym]

          optAndRep.add(getOpt(newIdentNode(innerSym), ty, new))
          nimyInfo[newStr] = initNimyRow(NonTerm,
                                         rtn = rt,
                                         rtp = genSym(nskConst))
          nimyInfo[innerSym] = NimyRow(
            kind: nr.kind,
            retTyNode: nr.retTyNode,
            ruleToProc: nr.ruleToProc,
            optRule: new,
            repRule: nr.repRule
            )

        of nnkCurlyExpr:
          if nimyInfo[innerSym].optRule.kind != nnkEmpty:
            continue
          # this bloody mess here is to rewrite the production
          # NUM{} into 
          # __rep_NUM -> Empty | __rep_NUM -> __inner_rep_NUM
          # and
          # __inner_rep_NUM -> Term(Num) | 
          # __inner_rep_NUM -> __inner_rep_NUM Term(Num)
          let
            newStr = "__rep_" & innerSym
            new = newIdentNode(newStr)
            newInnerStr = "__inner_" & newStr
            newInner = newIdentNode(newInnerStr)
            ty = if innerSym.isNonTerm(nimyInfo):
                   nimyInfo[innerSym].retTyNode
                 else:
                   tokenType
            rt = nnkBracketExpr.newTree(
              newIdentNode("seq"),
              ty
            )
            nr = nimyInfo[innerSym]

          optAndRep.add(getRep(newIdentNode(innerSym), ty, new, newInner))
          nimyInfo[newStr] = initNimyRow(NonTerm, rtn = rt,
                                         rtp = genSym(nskConst))
          nimyInfo[newInnerStr] = initNimyRow(NonTerm, rtn = rt,
                                              rtp = genSym(nskConst))
          nimyInfo[innerSym] = NimyRow(
            kind: nr.kind,
            retTyNode: nr.retTyNode,
            ruleToProc: nr.ruleToProc,
            optRule: nr.optRule,
            repRule: new
            )

        else:
          discard

  # make top clause proc
  let topClause = nnkCall.newTree(
    nnkBracketExpr.newTree(
      newIdentNode("__Start__"),
      returnType
    ),
    nnkStmtList.newTree(
      nnkCall.newTree(
        newIdentNode(topNonTerm),
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(1)
            )
          )
        )
      )
    )
  )

  # read BNF second (make procs)
  for i, clause in iter(topClause, body, optAndRep):
    if clause.kind notin {nnkCall, nnkCommand}:
      continue
    let
      (nonTerm, rType) = parseLeft(clause)
      ruleClauses = clause[1]
    var ruleToProcMakerBody = nnkStmtList.newTree(
      initRuleToProcNode(tokenType, tokenKind, rType)
    )

    # read Rule
    for j, ruleClause in ruleClauses:
      if ruleClause.kind == nnkCommentStmt:
        continue
      let
        left = nnkCall.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("NonTermS"),
            tokenKind
          ),
          newStrLitNode(nonTerm)
        )
        # argTypes: seq[string] (name if nonterm)
        (ruleMaker, argTypes, clauseBody, prec) = parseRuleAndBody(
          ruleClause, tokenKind, tokenType, left, nimyInfo, 
          precedence
        )
        ruleId = genSym(nskConst)
        ruleProcId = if i == 0:
                       topProcId
                     else:
                       genSym(nskProc)
      ruleIds.add(ruleId)
      let ruleDef = newConstStmt(
        ruleId,
        ruleMaker
      )
      # makeRule
      ruleDefs.add(
        ruleDef
      )

      # make proc and add to result
      ruleProcs.add(
        makeRuleProc(ruleProcId, clauseBody, nimyInfo[nonTerm].retTyNode,
                     tokenType, tokenKind, argTypes, nimyInfo)
      )
      ruleProcPts.add(
        makeRuleProc(ruleProcId, clauseBody, nimyInfo[nonTerm].retTyNode,
                     tokenType, tokenKind, argTypes, nimyInfo, true)
      )

      ruleToProcMakerBody.add(
        nnkAsgn.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("result"),
            ruleId
          ),
          ruleProcId
        )
      )
    # ruleToProcMakerDef
    let
      ruleToProcMakerName = genSym(nskProc)
      ruleToProcMakerNode = newProc(
        ruleToProcMakerName,
        @[nnkBracketExpr.newTree(
          newIdentNode("RuleToProc"),
          tokenType,
          tokenKind,
          rType
        )],
        ruleToProcMakerBody
      )
    ruleToProcMakers.add(
      ruleToProcMakerNode
    )
    # add table to result
    tableConstDefs.add(
      newConstStmt(
        nimyInfo[nonTerm].ruleToProc,
        nnkCall.newTree(
          ruleToProcMakerName
        )
      )
    )

  result.add(ruleDefs)
  result.add(ruleProcPts)
  result.add(ruleToProcMakers)
  result.add(tableConstDefs)
  result.add(ruleProcs)

  # makeGrammarAndParsingTable
  for nt in nimyInfo.keys:
    symNodes.add(convertToSymNode(nt, tokenKind, nimyInfo))
  symNodes.add(
    newCall(
      nnkBracketExpr.newTree(
        newIdentNode("End"),
        tokenKind
      )
    )
  )
  symNodes.add(
    newCall(
      nnkBracketExpr.newTree(
        newIdentNode("Empty"),
        tokenKind
      )
    )
  )
  let
    tmpName = genSym(nskProc)
  result.add(
    tableMakerProc(tmpName, tokenType, tokenKind, topNonTermNode, tableMaker,
                   ruleIds, symNodes, precedence)
  )
  when defined(nimylet):
    result.add(
      newLetStmt(
        nnkPostfix.newTree(
          newIdentNode("*"),
          parserName,
        ),
        nnkCall.newTree(
          tmpName
        )
      )
    )
  else:
    result.add(
      newConstStmt(
        nnkPostfix.newTree(
          newIdentNode("*"),
          parserName,
        ),
        nnkCall.newTree(
          tmpName
        )
      )
    )

  # add proc parse
  result.add quote do:
    proc parse*[T,S](parser: var Parser[S]; lexer: var NimlLexer[T]): `returnType` = 
      let tree = parseImpl(parser, lexer)
      return `topProcId`(tree)
  
  when defined(nimydebug):
    echo toStrLit(result)
