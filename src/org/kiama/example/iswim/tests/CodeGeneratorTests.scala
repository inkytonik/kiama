/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Dominic R B Verity, Macquarie University.
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
 * Tests of code generation attribution.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.kiama.util.PrettyPrinter
import org.kiama.example.iswim.compiler._
import org.kiama.example.iswim.secd._

@RunWith(classOf[JUnitRunner])
class CodeGeneratorTests extends FunSuite with CodeGenerator with SemanticAnalysis with Parser {

    import Syntax._

    import org.kiama.util.Messaging._

    import SECDBase._
    import IntegerOps._
    import BooleanOps._
    import IntComparisonOps._
    import StackOps._
    import StringOps._
    import HeapOps._
    import RecordOps._

    test("compile a simple arithmetic expression") {
        val prog = parseAll(expr, "10 + x * 42")
        assert(prog.successful)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            PushInt(10),
            Lookup("x"),
            PushInt(42),
            Mult(),
            Add()
        ))
    }

    test("compile a simple boolean expression") {
        val prog = parseAll(expr, "true & x | y")
        assert(prog.successful)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            PushTrue(),
            Test(CodeSegment(Lookup("x")),PushFalse()),
            Test(PushTrue(),CodeSegment(Lookup("y")))
        ))
    }

    test("compile a boolean comparison expression") {
        val prog = parseAll(expr, "(a == 20) | (b != 30) & (a > b)")
        assert(prog.successful)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            Lookup("a"),
            PushInt(20),
            Equals(),
            Test(PushTrue(),CodeSegment(
                Lookup("b"),
                PushInt(30),
                Equals(),
                Test(PushFalse(),PushTrue()),
                Test(CodeSegment(
                    Lookup("a"),
                    Lookup("b"),
                    Swap(1,1),
                    LessThan()
                ),PushFalse()))
            )
        ))
    }

    test("compile a let expression") {
        val prog = parseAll(expr,"let x = 10 and y = 22 + 11 in x * y")
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            PushInt(10),
            PushInt(22),
            PushInt(11),
            Add(),
            Enter(List("x","y")),
            Lookup("x"),
            Lookup("y"),
            Mult(),
            Exit()
        ))
    }

    test("compile a letrec expression") {
        val prog = parseAll(expr,"letrec f = fun(n) (n + 1) and g = fun(m) (m - 1) in f")
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            MkClosures(List(
                FunctionSpec(Some("f"),"n",CodeSegment(
                    Lookup("n"),
                    PushInt(1),
                    Add()
                )),
                FunctionSpec(Some("g"),"m",CodeSegment(
                    Lookup("m"),
                    PushInt(1),
                    Sub()
                ))
            )),
            Enter(List("f","g")),
            Lookup("f"),
            Exit()
        ))
    }

    test("compile a tuple expressio") {
        val prog = parseAll(expr,"(10,20,(),30)")
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            PushInt(10),
            PushInt(20),
            PushEmpty(),
            PushInt(30),
            MkRecord(4)
        ))
    }

    test("compile a let binding of a lambda expression") {
        val prog = parseAll(expr,"let f = fun(n) (n + 1) and g = fun(m) (m - 1) in f(g(10))")
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            MkClosures(List(
                FunctionSpec(None,"n",CodeSegment(
                    Lookup("n"),
                    PushInt(1),
                    Add()
                ))
            )),
            MkClosures(List(
                FunctionSpec(None,"m",CodeSegment(
                    Lookup("m"),
                    PushInt(1),
                    Sub()
                ))
            )),
            Enter(List("f","g")),
            PushInt(10),
            Lookup("g"),
            App(),
            Lookup("f"),
            App(),
            Exit()
        ))
    }

    test("compile some function applications") {
        val prog = parseAll(expr,"f(10 + g(h(k)))")
        assert(prog.successful)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            PushInt(10),
            Lookup("k"),
            Lookup("h"),
            App(),
            Lookup("g"),
            App(),
            Add(),
            Lookup("f"),
            App()
        ))
    }

    test("compile a block expression") {
        val prog = parseAll(expr,"{10;20;\"hello\";();22} + {true}")
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            PushInt(10),
            Pop(1),
            PushInt(20),
            Pop(1),
            PushString("hello"),
            Pop(1),
            PushEmpty(),
            Pop(1),
            PushInt(22),
            PushTrue(),
            Add()
        ))
    }

    test("compile a simple while loop - count from 1 to 20") {
        val prog = parseAll(expr,"""
    let c = mkref 0
    in while (val c < 20) {
        c := val c + 1
    }
""")
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        assert(result === CodeSegment(
            Alloc(),
            Dup(1),
            PushInt(0),
            Put(),
            Enter(List("c")),
            MkClosures(List(
                FunctionSpec(None,"@loop", CodeTree(
                    Lookup("c"),
                    Get(),
                    PushInt(20),
                    LessThan(),
                    Test(
                        CodeTree(
                            Lookup("c"),
                            Lookup("c"),
                            Get(),
                            PushInt(1),
                            Add(),
                            Dup(1),
                            Swap(1,2),
                            Put(),
                            Lookup("@loop"),
                            Dup(1),
                            TailApp()
                        ),
                        PushEmpty()
                    )
                ))
            )),
            Dup(1),
            App(),
            Exit()
        ))
    }

    test("compile a simple match expression") {
        val prog = parseAll(expr,"""
    (1,2) match {
        (x,y,z)     -> x * y + z;
        ()          -> 20
    }
""")
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        val p : PrettyPrinter = new PrettyPrinter
        result.pretty(p)
        assert(p.toString === """CodeSegment(
1:  PushInt(1),
2:  PushInt(2),
3:  MkRecord(2),
4:  Dup(1),
5:  GetType(),
6:  Dup(1),
7:  PushType(RecordTypeValue),
8:  Equals(),
9:  Swap(1,1),
10: PushType(EmptyTypeValue),
11: Equals(),
12: Test(
        CodeSegment(
13:         Pop(1),
14:         PushInt(0)
        ),
        CodeSegment(
15:         Test(
                CodeSegment(
16:                 Dup(1),
17:                 Fields()
                ),
                CodeSegment(
18:                 PushInt(1)
                )
            )
        )
    ),
19: Dup(1),
20: PushInt(3),
21: Equals(),
22: Test(
        CodeSegment(
23:         Pop(1),
24:         UnpackRecord(),
25:         Enter(List(x, y, z)),
26:         Lookup(x),
27:         Lookup(y),
28:         Mult(),
29:         Lookup(z),
30:         Add(),
31:         Exit()
        ),
        CodeSegment(
32:         Dup(1),
33:         PushInt(0),
34:         Equals(),
35:         Test(
                CodeSegment(
36:                 Pop(2),
37:                 PushInt(20)
                ),
                CodeSegment(
38:                 PushMachineException(MachineExceptionValue: match error, value not matched at <undefined position>),
39:                 RaiseException()
                )
            )
        )
    )
)""")
    }

    test("compile a simple, but complete, program") {
        val prog = parseAll(start, """
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
        assert(prog.successful)
        assert((prog.get)->isSemanticallyCorrect)
        val result : CodeSegment = code(prog.get)
        val p : PrettyPrinter = new PrettyPrinter
        result.pretty(p)
        assert(p.toString === """CodeSegment(
1:  BindPrims(List(write, read, fields, type)),
2:  MkClosures(
        FunctionSpec(
            fib,
            n,
            CodeSegment(
3:              Alloc(),
4:              Dup(1),
5:              PushInt(0),
6:              Put(),
7:              Alloc(),
8:              Dup(1),
9:              PushInt(1),
10:             Put(),
11:             Alloc(),
12:             Dup(1),
13:             PushInt(0),
14:             PushInt(1),
15:             Sub(),
16:             Put(),
17:             Enter(List(r1, r2, r3)),
18:             MkClosures(
                    FunctionSpec(
                        f,
                        m,
                        CodeSegment(
19:                         Lookup(m),
20:                         PushInt(0),
21:                         Equals(),
22:                         Test(
                                CodeSegment(
23:                                 Lookup(r1),
24:                                 Get()
                                ),
                                CodeSegment(
25:                                 Lookup(r3),
26:                                 Lookup(r1),
27:                                 Get(),
28:                                 Lookup(r2),
29:                                 Get(),
30:                                 Add(),
31:                                 Dup(1),
32:                                 Swap(1,2),
33:                                 Put(),
34:                                 Pop(1),
35:                                 Lookup(r1),
36:                                 Lookup(r2),
37:                                 Get(),
38:                                 Dup(1),
39:                                 Swap(1,2),
40:                                 Put(),
41:                                 Pop(1),
42:                                 Lookup(r2),
43:                                 Lookup(r3),
44:                                 Get(),
45:                                 Dup(1),
46:                                 Swap(1,2),
47:                                 Put(),
48:                                 Pop(1),
49:                                 Lookup(m),
50:                                 PushInt(1),
51:                                 Sub(),
52:                                 Lookup(f),
53:                                 App()
                                )
                            )
                        )
                    )
                ),
54:             Enter(List(f)),
55:             Lookup(n),
56:             Lookup(f),
57:             App(),
58:             Exit(),
59:             Exit()
            )
        )
    ),
60: Enter(List(fib)),
61: PushInt(200),
62: Lookup(fib),
63: App(),
64: Lookup(write),
65: App(),
66: Pop(1),
67: PushString(\n),
68: Lookup(write),
69: App(),
70: Exit(),
71: Exit()
)""")
   }
}
