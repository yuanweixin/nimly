import unittest
import nimly/parsetypes
import std/options
import std/sets
import std/tables
import nimly/debuginfo

func NT(s: string) : Symbol[string] = 
    return NonTermS[string](s)

func T(s: string) : Symbol[string] = 
    return TermS[string](s)

func E() : Symbol[string] = 
    return ErrorS[string]()

func Shift(i: int) : ActionTableItem[string] = 
    return ActionTableItem[string](kind: ActionTableItemKind.Shift, state: i)

func Reduce(r: Rule[string]) : ActionTableItem[string] = 
    return ActionTableItem[string](kind: ActionTableItemKind.Reduce, rule: r)

func Accept() : ActionTableItem[string] = 
    return ActionTableItem[string](kind : ActionTableItemKind.Accept)

func Err() : ActionTableItem[string] = 
    return ActionTableItem[string](kind:ActionTableItemKind.Error)

suite "LALRItems":
    var 
        state = 0
        r1 = newRule(none[Precedence](), NT("S"), NT("C"),NT("C"))
        r2 = newRule(none[Precedence](), NT("C"), T("c"), NT("C"))
        r3 = newRule(none[Precedence](), NT("C"), T("d"))
        r4 = newRule(none[Precedence](), NT("C"), E(), T("d"), T("x"))
        g = initGrammar[string]([r1,r2,r3,r4].toOrderedSet, NT("S")).augment
        items =  [
            LALRItem[string](rule: g.startRule, pos: 0, ahead: End[string]()),
            LALRItem[string]( # S -> C C . [$]
                rule: newRule(none[Precedence](), NT("S"),
                            NT("C"),NT("C")),
                pos: 0,
                ahead: End[string]()
            ),
            LALRItem[string]( # C -> . c C [c]
                rule: newRule(none[Precedence](), NT("C"), T("c"), NT("C")),
                pos: 0,
                ahead: T("c")
            ),
            LALRItem[string]( # C -> c . C [d]
                rule: newRule(none[Precedence](), NT("C"), T("c"), NT("C")),
                pos: 0,
                ahead: T("d")
            ),
            LALRItem[string]( # C -> . d [c]
                rule: newRule(none[Precedence](), NT("C"), T("d")),
                pos: 0,
                ahead: T("c")
            ),
            LALRItem[string]( # C -> . d [d]
                rule: newRule(none[Precedence](), NT("C"), T("d")),
                pos: 0,
                ahead: T("d")
            ),
            LALRItem[string]( # C -> . d [d]
                rule: newRule(none[Precedence](), NT("C"), T("d")),
                pos: 0,
                ahead: T("d")
            ),
            LALRItem[string]( # C -> . d x [x]
                rule: newRule(none[Precedence](), NT("C"), T("d"), T("x")),
                pos: 0,
                ahead: E()
            )
        ].toHashSet
        action = {
            state: {
                T("c") : Shift(1),
                T("d") : Reduce(r3),
                End[string]() : Accept(), 
                T("x") : Err()
            }.toTable
        }.toTable
        goto = {
            state: {
                NT("C"): 2,
                E() : 3 
            }.toTable
        }.toTable

    test "LALRItems string":
        discard

    test "LALRItems dot":
        discard

suite "LRItems":
    test "LRItems string":
        discard

    test "LRItems dot":
        discard 