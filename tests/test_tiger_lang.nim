import unittest
import patty 
import nimyacc
import strutils
import common

# Based on the grammar given here:
# https://www.cs.princeton.edu/~appel/modern/ml/chap3/tiger.grm

variantp Token:
    Type
    Var
    Function
    Break
    Of
    End
    In
    Nil
    Let
    Do
    To
    For
    While
    Else
    Then
    If
    Array
    Assign
    Or
    And
    Ge
    Gt
    Le
    Lt
    Neq
    Eq
    Divide
    Times
    Minus
    Plus
    Dot
    Rbrace
    LBrace
    Rbrack
    Lbrack
    Rparen
    Lparen
    Semicolon
    Colon
    Comma
    String(s: string)
    Int(i: int)
    Id(id: string)
    Eof


genStringMatcher testLex[LexerState, Token]:
    r"\n":
        discard
    r"\s":
        discard
    r"type":
        yield Type()
    r"var":
        yield Var()
    r"function":
        yield Function()
    r"break":
        yield Break()
    r"of":
        yield Of()
    r"end":
        yield End()
    r"in":
        yield In()
    r"nil":
        yield Nil()
    r"let":
        yield Let()
    r"do":
        yield Do()
    r"to":
        yield To()
    r"for":
        yield For()
    r"while":
        yield While()
    r"else":
        yield Else()
    r"then":
        yield Then()
    r"if":
        yield If()
    r"array":
        yield Array()
    r"\:=":
        yield Assign()
    r"\|":
        yield Or()
    r"&":
        yield And()
    r">=":
        yield Ge()
    r">":
        yield Gt()
    r"<=":
        yield Le()
    r"<":
        yield Lt()
    r"=":
        yield Eq()
    r"<>":
        yield Neq()
    r"/":
        yield Divide()
    r"\*":
        yield Times()
    r"-":
        yield Minus()
    r"\+":
        yield Plus()
    r"\.":
        yield Dot()
    r"\}":
        yield Rbrace()
    r"\{":
        yield Lbrace()
    r"\]":
        yield Rbrack()
    r"\[":
        yield Lbrack()
    r"\)":
        yield Rparen()
    r"\(":
        yield Lparen()
    r"\:":
        yield Colon()
    r";":
        yield Semicolon()
    r",":
        yield Comma()
    """["]""":
        beginState(string)
    string:
        """["]""":
            beginState(initial)
            yield String(lexState.strBody)
            lexState.strBody = ""
        r"\\t":
            lexState.strBody.add "\t"
        r"\\n":
            lexState.strBody.add "\n"
        """\\\"""":
            lexState.strBody.add "\""
        r"\\\\":
            lexState.strBody.add "\\"
        r"\\b":
            lexState.strBody.add "\b"
        r"\\r":
            lexState.strBody.add "\r"
        r"\\f":
            lexState.strBody.add "\f"
        r"\\[0-9]{3,3}":
            let i = parseInt(input.substr(oldPos+1, pos-1))
            lexState.strBody.add $chr(i)
        """\\(\t|\f|\n| )+\\""":
            discard
        r".":
            lexState.strBody.add input.substr(oldPos, pos-1)
    comment:
        r"/\*":
            inc lexState.commentDepth
        r"\*/":
            dec lexState.commentDepth
            if lexState.commentDepth == 0:
                beginState(initial)
        r".":
            discard
    r"/\*":
        inc lexState.commentDepth
        beginState(comment)
    r"[0-9]+":
        yield Int(parseInt(input.substr(oldPos, pos-1)))
    r"[a-zA-Z][a-zA-Z_0-9]*":
        yield Id(input.substr(oldPos, pos-1))
    "\0":
        yield Eof()
    r".":
        raise newException(Exception, "Unexpected character###" & input.substr(
                oldPos, pos-1) & "### at [" & $oldPos & "," & $(pos-1) & "]")

nimy testPar[Token]:
    %nonassoc Eq Neq Lt Le Gt Ge 
    %left Plus Minus 
    %left Times Divide
    %left Uminus 
    prog[string]:
       exp:
        return $1  
    exp[string]:
        Nil:
            return "TODO"
        Int:
            return "TODO"
        String:
            return "TODO"
        Id Lbrack exp Rbrack Of exp:
            return "TODO"
        Id Lbrace field_value_list Rbrace:
            return "TODO"
        lvalue:
            return "TODO"
        lvalue Assign exp:
            return "TODO"
        exp Lparen args Rparen:
            return "TODO"
        exp Or exp:
            return "TODO"
        exp And exp:
            return "TODO"
        exp Eq exp:
            return "TODO"
        exp Neq exp:
            return "TODO"
        exp Lt exp:
            return "TODO"
        exp Le exp:
            return "TODO"
        exp Gt exp:
            return "TODO"
        exp Ge exp:
            return "TODO"
        exp Plus exp:
            return "TODO"
        exp Minus exp:
            return "TODO"
        exp Times exp:
            return "TODO"
        exp Divide exp:
            return "TODO"
        Minus exp %prec Uminus:
            return "TODO"
        If exp Then exp Else exp: 
            return "TODO"
        If exp Then exp: 
            return "TODO"
        While exp Do exp: 
            return "TODO"
        For Id Assign exp To exp Do exp:
            return "TODO"
        Break:
            return "TODO"
        Let decs In sequence End:
            return "TODO"
        Lparen sequence Rparen:
            return "TODO"
    field_value_list[string]:
        Id Eq exp:
            return "TODO"
        Id Eq exp Comma field_value_list:
            return "TODO"
    lvalue[string]:
        Id: 
            return "TODO"
        Id lvaluetail:
            return "TODO"
    lvaluetail[string]:
        []:
            return "TODO"
        Dot Id lvaluetail:
            return "TODO"
        Lbrack exp Rbrack lvaluetail:
            return "TODO"
    fields[string]:
        []:
            return "TODO"
        Id Colon Id tyfieldstail:
            return "TODO"
    tyfieldstail[string]:
        []:
            return "TODO"
        Comma Id Colon Id tyfieldstail:
            return "TODO"
    sequence[string]:
        []:
            return "TODO"
        exp:
            return "TODO"
        exp Semicolon sequence:
            return "TODO"
    args[string]:
        []:
            return "TODO"
        exp:
            return "TODO"
        exp Comma args_rest:
            return "TODO"
    args_rest[string]:
        exp:
            return "TODO"
        exp Comma args_rest:
            return "TODO"
    type_opt[string]:
        []:
            return "TODO"
        Colon Id:
            return "TODO"
    vardec[string]:
        Var Id type_opt Assign exp:
            return "TODO"
    fundec[string]:
        Function Id Lparen fields Rparen type_opt Eq exp:
            return "TODO"
        Function Id Lparen fields Rparen type_opt Eq exp fundec:
            return "TODO"
    tydec[string]:
        Type Id Eq ty:
            return "TODO"
        Type Id Eq ty tydec:
            return "TODO"
    ty[string]:
        Id:
            return "TODO"
        Lbrace fields Rbrace:
            return "TODO"
        Array Of Id:
            return "TODO"
    decs[string]:
        dec decs:
            return "TODO"
        []:
            return "TODO"
    dec[string]:
        vardec:
            return $1
        fundec:
            return $1
        tydec:
            return $1

test "parses merge.tig":
    var state : LexerState
    let input = readFile("tests/merge.tig")
    var lexer = testLex.newWithString(state, input)
    var parser = testPar.newParser()
    discard parser.parse_testPar(lexer)
    doAssert not parser.hasError


test "parses queens.tig":
    let input = readFile("tests/queens.tig")
    discard