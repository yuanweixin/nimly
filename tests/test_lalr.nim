import unittest
import options
include nimyacc/slr
include nimyacc/lalr
import sequtils

proc newRule*(prec: Option[Precedence], left: Symbol, right: varargs[Symbol], index: int): Rule =
  result = Rule(left: left, right: right.toSeq, prec: prec, index:index)

proc newRule*(prec: Option[Precedence], left: Symbol, right: Symbol, index:int): Rule =
  result = Rule(left: left, right: @[right], prec: prec, index:index)


let
  g = initGrammar(
    [
      newRule(none[Precedence](), NonTermS("S"),
              NonTermS("C"),NonTermS("C"), 1),
      newRule(none[Precedence](), NonTermS("C"),TermS(1), NonTermS("C"), 2),
      newRule(none[Precedence](), NonTermS("C"),TermS(2), 3),
    ],
    NonTermS("S")
  ).augment

  g415 = initGrammar(
    [
      newRule(none[Precedence](), NonTermS("S"), NonTermS("R"), 1),
      newRule(none[Precedence](), NonTermS("S"),
              NonTermS("L"),TermS(3), NonTermS("R"), 2),
      newRule(none[Precedence](), NonTermS("L"),TermS(4), NonTermS("R"), 3),
      newRule(none[Precedence](), NonTermS("L"),TermS(5), 4),
      newRule(none[Precedence](), NonTermS("R"), NonTermS("L"), 5),
    ],
    NonTermS("S")
  ).augment

test "test closure for lalr":
  let
    itm = LALRItem(ruleIdx: 0, pos: 0, ahead: End(), g:g)
    c = closure(g, toHashSet[LALRItem]([itm]))
    expected =  [
      itm,
      LALRItem(
        ruleIdx: 1,
        pos: 0,
        ahead: End(),
        g: g
      ),
      LALRItem(
        ruleIdx: 2, 
        pos: 0,
        ahead:TermS(1),
        g: g
      ),
      LALRItem(
        ruleIdx: 2,
        pos: 0,
        ahead:TermS(2),
        g: g
      ),
      LALRItem(
        ruleIdx: 3,
        pos: 0,
        ahead:TermS(1),
        g: g
      ),
      LALRItem(
        ruleIdx: 3, 
        pos: 0,
        ahead:TermS(2),
        g: g
      )
    ].toHashSet

  check expected == c

proc contains(itms: LALRItems, itm: LRItem): bool =
  result = false
  for i in itms:
    if i.toLRItem == itm:
      return true

test "test make LALR kernel":
  let
    (cc, tt) = g415.makeCanonicalCollection
    kernel = cc.filterKernel
  check kernel.card == 10
  for i, itms in kernel:
    if itms.contains(
      LRItem(
        ruleIdx: 2,
        pos: 1,
        g: g415
      )
    ):
      check itms.card == 2
    else:
      check itms.card == 1
  let lalrKernel = kernel.toLALRKernel(g415, tt)
  for i, itms in lalrKernel:
    if itms.contains(
      LRItem(
        ruleIdx: 5,
        pos: 1, 
        g: g415
      )
    ) or itms.contains(
      LRItem(
        ruleIdx: 3,
        pos: 1,
        g: g415
      )
    ) or itms.contains(
      LRItem(
        ruleIdx: 4,
        pos: 1,
        g: g415
      )
    ) or itms.contains(
      LRItem(
        ruleIdx: 3,
        pos: 2,
        g: g415
      )
    ):
      check itms.card == 2
    else:
      check itms.card == 1


