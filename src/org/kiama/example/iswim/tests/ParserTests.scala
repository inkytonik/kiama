/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Dominic R B Verity, Anthony Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.kiama
package example.iswim.tests

/*
 * Tests of the ISWIM parser combinators.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.kiama.example.iswim.compiler._

@RunWith(classOf[JUnitRunner])
class ParserTests extends FunSuite with Parser {

    import Syntax._

    /**
     * Assert that a parse did not work at a given line and column with a given
     * message.
     */
    def assertNoSucess[T] (result : ParseResult[T], line : Int, column : Int,
                        msg : String) {
        result match {
            case e : NoSuccess =>
                expect (line, "wrong line number in error") (e.next.pos.line)
                expect (column, "wrong column number in error") (e.next.pos.column)
                expect (msg, "wrong message in error") (e.msg)
            case _         =>
                fail ("parse succeeded, but should have failed")
        }
    }

    test("parse a 0-tuple") {
        val result = parseAll(expr, "()")
        assert(result.successful)
        assert(result.get === Empty())
    }

    test("parse a 2-tuple of manifest boolean values") {
        val result = parseAll(expr, "(true,true)")
        assert(result.successful)
        assert(result.get === Tuple(List(BoolVal(true),BoolVal(true))))
    }

    test("parse a 4-tuple of manifest boolean values") {
        val result = parseAll(expr, "(true,false,false,true)")
        assert(result.successful)
        assert(result.get ===
            Tuple(List(BoolVal(true),BoolVal(false),
                        BoolVal(false),BoolVal(true))))
    }

    test("parse nested tuples of manifest boolean values") {
        val result = parseAll(expr, "(true,(false,false),true)")
        assert(result.successful)
        assert(result.get ===
            Tuple(List(
                BoolVal(true),
                Tuple(List(BoolVal(false),BoolVal(false))),
                BoolVal(true))))
    }

    test("parse error: tuple missing open bracket") {
        val result = parseAll(expr, "true,false)")
        assertNoSucess(result, 1, 5, """string matching regex `\z' expected but `,' found""")
    }

    test("parse error: tuple missing close bracket") {
        val result = parseAll(expr, "(true,()")
        assertNoSucess(result, 1, 9, """operator ")" expected""")
    }

    test("parse a tuple of literal integers and boolean values") {
        val result = parseAll(expr, "(1,true,3,-5)")
        assert(result.successful)
    }

    test("parse a tuple of literal integers and boolean values and verify result") {
        val result = parseAll(expr, "(1,true,3,-5)")
        assert(result.get ===
            Tuple(List(NumVal(1),BoolVal(true),
                        NumVal(3),Negate(NumVal(5)))))
    }

    test("parse a simple arithmetic expression") {
        val result = parseAll(expr,"1 + 2 * 3 + 4 * (5- 4 + 7 % -2) / 6")
        assert(result.successful)
    }

    test("parse a simple arithmetic expression and verify result") {
        val result = parseAll(expr,"1+2*3+4*(dom-4+7%-2)/6")
        assert(result.get ===
            Plus(Plus(NumVal(1),Times(NumVal(2),NumVal(3))),
                 Divide(Times(NumVal(4),
                              Plus(Minus(Variable("dom"),NumVal(4)),
                                   Remainder(NumVal(7),Negate(NumVal(2))))),
                        NumVal(6))))
    }

    test("attempt to parse a keyword as a variable name") {
        val result = parseAll(variable, "callcc")
        assertNoSucess(result, 1, 7, """keyword "callcc" found where variable name expected""")
    }

    test("parse a variable name whose prefix is a keyword") {
        val result = parseAll(variable, "elseifvar")
        assert(result.successful)
    }

    test("attempted parse of non-matching keyword") {
        val result = parseAll(keyword("if"),"else")
        assertNoSucess(result, 1, 5, """keyword "if" expected""")
    }

    test("attempted parse of matching keyword from front of variable name") {
        val result = parseAll(keyword("else"),"elseifvar")
        assertNoSucess(result, 1, 10, """keyword "else" expected""")
    }

    test("parse a match clause") {
        val result = parseAll(matchclause, "(dom,sal) -> sal * dom + 1")
        assert(result.successful)
        assert(result.get ===
            MatchClause(Pattern(List(Variable("dom"),Variable("sal"))),
                Plus(Times(Variable("sal"),Variable("dom")),NumVal(1))))
    }

    test("attempt to parse a match clause with a bad pattern") {
        val result = parseAll(matchclause, "(1,x) -> 1")
        assertNoSucess(result, 1, 2, "variable name expected")
    }

    test("attempt to parse a letrec which binds a non-lambda expression") {
        val result = parseAll(expr, "letrec x = fun(y) (x y) and z = x in (x 10)")
        assertNoSucess(result, 1, 34, """keyword "fun" expected""")
    }

    test("parse a match expression") {
        val result = parseAll(expr, """
(dom 10) match {
    ()      -> 0;
    x       -> x;
    (x,y,z) -> x * y + z
}
""")
        assert(result.successful)
        assert(result.get ===
            Match(Apply(Variable("dom"),NumVal(10)),
                  List(MatchClause(Pattern(List()),NumVal(0)),
                       MatchClause(Pattern(List(Variable("x"))),Variable("x")),
                       MatchClause(Pattern(List(Variable("x"), Variable("y"), Variable("z"))),
                                   Plus(Times(Variable("x"),Variable("y")),Variable("z"))))))
    }

    test("parse a sequence of function applications") {
        val result = parseAll(expr, "dom 10 (sal + flo * 10) herb (10,30)")
        assert(result.successful)
        assert(result.get ===
            Apply(Apply(Apply(Apply(Variable("dom"),
                                    NumVal(10)),
                              Plus(Variable("sal"),
                                   Times(Variable("flo"),NumVal(10)))),
                        Variable("herb")),
                  Tuple(List(NumVal(10),NumVal(30)))))
    }

    test("parse a code block") {
        val result = parseAll(expr, "{ 10 ; 15 * dom }")
        assert(result.successful)
        assert(result.get ===
            Block(List(NumVal(10),
                       Times(NumVal(15),Variable("dom")))))
    }

    test("parse application of a function to a code block") {
        val result = parseAll(expr, "domFun10 { 10 ; test }")
        assert(result.successful)
        assert(result.get ===
            Apply(Variable("domFun10"),
                  Block(List(NumVal(10),Variable("test"))))
            )
    }

    test("parse if ... else if ... else ... expression") {
        val result = parseAll(expr, """
if (inp == 20)
    test * 10 + 5
else if (inp <= 40)
    (test + 5) * hello
else if (inp >= 200)
    test % 4
else
    10
        """)
        assert(result.successful)
        assert(result.get ===
            If(
                Equal(Variable("inp"),NumVal(20))
            ,   Plus(Times(Variable("test"),NumVal(10)),NumVal(5))
            ,   If (
                    LessEq(Variable("inp"),NumVal(40))
                ,   Times(Plus(Variable("test"),NumVal(5)),Variable("hello"))
                ,   If (
                        GreaterEq(Variable("inp"),NumVal(200))
                    ,   Remainder(Variable("test"),NumVal(4))
                    ,   NumVal(10)
                    )
                )
            )
        )
    }

    test("parse a while expression") {
        val result = parseAll(expr, """
{
    i := 0;
    while (i <= 20)
        i := i + 1
}
        """)
    assert(result.successful)
    }

    test("parse a callcc expression") {
        val result = parseAll(expr, "10 + callcc dom")
        assert(result.successful)
        assert(result.get ===
            Plus(NumVal(10),CallCC(Variable("dom"))))
    }

    test("parse throw...to expression") {
        val result = parseAll(expr, "10 + throw v to c * 20")
        assert(result.successful)
        assert(result.get ===
            Plus(NumVal(10),Times(ThrowTo(Variable("v"),
                Variable("c")),NumVal(20))))
    }

    test("parse some other builtins") {
        val result = parseAll(expr,
            """{ r := mkref 100; 10 + val r * 20; write dom }""")
        assert(result.successful)
        assert(result.get ==
            Block(List(Assign(Variable("r"),MkRef(NumVal(100))),
                       Plus(NumVal(10),Times(Val(Variable("r")),NumVal(20))),
                       Apply(Variable("write"),Variable("dom")))))
    }

    test("parse a lambda expression") {
        val result = parseAll(expr,
            "fun(x) { write 10; return (x+1) }")
        assert(result.successful)
        assert(result.get ===
            Lambda(Variable("x"),Block(List(Apply(Variable("write"),NumVal(10)),
                                            Return(Plus(Variable("x"),NumVal(1)))))))
    }

    test("parse a simple correct let expression") {
        val result = parseAll(expr, "let a = 1 and b = 2 in a * b")
        assert(result.successful)
    }

    test("parse a correct letrec expression") {
        val result = parseAll(expr, """
letrec  plusone = fun(n) { n + 1 }
and     factorial = fun(n) { if (n == 0) 1 else n * factorial (n-1) }
in      factorial(plusone 5)
        """)
        assert(result.successful)
        assert(result.get ===
            LetRec(List(
                Binding(Variable("plusone"),
                        Lambda(Variable("n"),
                               Block(List(Plus(Variable("n"),NumVal(1)))))),
                Binding(Variable("factorial"),
                        Lambda(Variable("n"), Block(List(
                            If(Equal(Variable("n"),NumVal(0)),NumVal(1),
                               Times(Variable("n"),Apply(Variable("factorial"),
                                                          Minus(Variable("n"),NumVal(1)))))))))),
                Apply(Variable("factorial"),Apply(Variable("plusone"),NumVal(5)))))
    }

    test("attempted parse of a letrec with a binding whose rhs is not a lambda clause") {
        val result = parseAll(expr, "letrec a = fun(n) n + 1 and b = 22 in (a b)")
        assertNoSucess(result, 1, 33, "keyword expected")
    }

    test("parse an expression containing a string literal") {
        val result = parseAll(expr, """{ write "hello\n"; write "there!\n" }""")
        assert(result.successful)
        assert(result.get ===
            Block(List(Apply(Variable("write"),StringVal("hello\\n")),
                       Apply(Variable("write"),StringVal("there!\\n")))))
    }

    test("check that the expression parser correctly handles comments") {
        val result = parseAll(expr, """
/* put */ fun /* a */ (/*comment*/x/*wherever*/)/* you */ { /* like */ write
/* and */ 10 /* everything */; /* should */ return /* still */(/*parse*/x
/*just   */+/* perfectly */1/*dontcha*/)/* know */ } /* old bean */
        """)
        assert(result.successful)
        assert(result.get ===
            Lambda(Variable("x"),Block(List(Apply(Variable("write"),NumVal(10)),
                                            Return(Plus(Variable("x"),NumVal(1)))))))
    }

    test("parse a simple, but complete, program") {
        val result = parseAll(start, """
        /*
         * Title:       Fibonacci fun
         * Description: A very simple imperative Fibonacci function with driver.
         * Copyright:   (c) 2010 Dominic Verity, Macquarie University
         */

        // declare preloaded primitives
        primitives write, read, fields, type;

        // Imperative fibonacci function
        letrec fib = fun(n)
            let r1 = mkref 0
            and r2 = mkref 1
            and r3 = mkref (-1)
            in  letrec f = fun(m)
                    if (m == 0)
                        val r1
                    else {
                        r3 := val r1 + val r2;
                        r1 := val r2;
                        r2 := val r3;
                        f (m-1) }
                in f n;

        // Execute an example and print the result.
        {
            write (fib 200);
            write "\n"
        }
""")
        assert(result.successful)
        assert(result.get ===
            IswimProg(List(
              Primitives(List(
                  Variable("write"),Variable("read"),
                  Variable("fields"),Variable("type")))
            , LetRecStmt(List(
                Binding(
                  Variable("fib")
                , Lambda(
                    Variable("n")
                  , Let(
                      List(
                        Binding(Variable("r1"),MkRef(NumVal(0)))
                      , Binding(Variable("r2"),MkRef(NumVal(1)))
                      , Binding(Variable("r3"),MkRef(Negate(NumVal(1)))))
                    , LetRec(
                        List(
                          Binding(
                            Variable("f")
                          , Lambda(
                              Variable("m")
                            , If(
                                Equal(Variable("m"),NumVal(0))
                              , Val(Variable("r1"))
                              , Block(List(
                                  Assign(
                                    Variable("r3")
                                  , Plus(Val(Variable("r1")),Val(Variable("r2"))))
                                , Assign(Variable("r1"),Val(Variable("r2")))
                                , Assign(Variable("r2"),Val(Variable("r3")))
                                , Apply(Variable("f"),Minus(Variable("m"),NumVal(1)))))))))
                      , Apply(Variable("f"),Variable("n"))))))))
            , ExprStmt(Block(List(
                Apply(Variable("write"),Apply(Variable("fib"),NumVal(200)))
              , Apply(Variable("write"),StringVal("\\n")))))
            )))
    }

    test("check that the parser can handle very long comments") {
        val result = parseAll(start, """
/**
 * Title:     An implementation of try...catch exception
 *            handling using continuations
 * Author:    Dominic Verity
 * Copyright: Macquarie University (c) 2007-2010
 *
 * A try..catch block is implemented using 2 continuations:
 *        The first marks the exit point for normal exits.
 *        The second marks the entry point to the exception handler code.
 *
 * We might picture this arrangement as:
 *
 *  --- normal exit cont    --- error handler cont
 *   |                       |
 *   |                       |                    code in the try block
 *   |                       |
 *   |                       |         at the end of the try block throw to normal
 *   |                       V         exit continuation to avoid exectuing the exception
 *   |                      ___        handler after successful execution of try code.
 *   |
 *   |
 *   |                                         code of corresponding
 *   |                                         exception handler.
 *   V
 *  ---
 *
 * The SECD machine provides a primitive, called 'raise', which allows us
 * to raise an exception. This can be brought into scope in user code using the 'primitives'
 * key word. The same mechanism also allows us to bring the names of the exception
 * values associated with the various machine exceptions into scope. Having done that we can
 * write code to catch and handle different kinds of machine error.
 *
 * The SECD 'raise' primitive assumes that when it is called a variable named 'exceptionHandler'
 * will be in scope and that it will be bound to a continuation. All it actually does is to
 * throw the exception value given as its argument to this continuation. The default provided
 * by the machine on startup is that 'exceptionHandler' is bound to a continuation marking the
 * exit point of the running program.
 *
 * We can replace this simple "exit on error" mechanism with something a little more useful
 * simply by re-binding 'exceptionHandler' to a more useful continuation. In the code given here
 * we organise things so that 'exceptionHandler' is bound always bound to the continuation
 * which marks the entry point to the catch block of the closest enclosing try...catch construct.
 */
""")
        assertNoSucess(result, 45, 1, """operator "{" expected""")
    }
}
