/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2016 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.minijava

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
class SyntaxAnalyserTests extends ParseTests {

    import MiniJavaTree._

    val parsers = new SyntaxAnalyser(positions)
    import parsers._

    def parseToExp(e : Expression) = parseTo(e)
    def parseToStmt(s : Statement) = parseTo(s)

    // Tests of parsing terminals

    test("parsing an identifier of one letter works") {
        identifier("x") should parseTo("x")
    }

    test("parsing an identifier as an identifier works") {
        identifier("count") should parseTo("count")
    }

    test("parsing an identifier containing digits and underscores works") {
        identifier("x1_2_3") should parseTo("x1_2_3")
    }

    test("parsing an integer as an identifier gives an error") {
        identifier("42") should failParseAt(1, 1,
            "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
    }

    test("parsing a non-identifier as an identifier gives an error (digit)") {
        identifier("4foo") should failParseAt(1, 1,
            "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
    }

    test("parsing a non-identifier as an identifier gives an error (underscore)") {
        identifier("_f3") should failParseAt(1, 1,
            "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '_' found")
    }

    test("parsing an integer of one digit as an integer works") {
        integer("8") should parseTo("8")
    }

    test("parsing an integer as an integer works") {
        integer("99") should parseTo("99")
    }

    test("parsing a non-integer as an integer gives an error") {
        integer("total") should failParseAt(1, 1,
            "string matching regex '[0-9]+' expected but 't' found")
    }

    // Tests of parsing basic expressions

    test("parsing an and expression produces the correct tree") {
        expression("a && 1") should parseToExp(AndExp(IdnExp(IdnUse("a")), IntExp(1)))
    }

    test("parsing a less then expression produces the correct tree") {
        expression("a < 1") should parseToExp(LessExp(IdnExp(IdnUse("a")), IntExp(1)))
    }

    test("parsing an addition expression produces the correct tree") {
        expression("a + 1") should parseToExp(PlusExp(IdnExp(IdnUse("a")), IntExp(1)))
    }

    test("parsing a subtraction expression produces the correct tree") {
        expression("a - 1") should parseToExp(MinusExp(IdnExp(IdnUse("a")), IntExp(1)))
    }

    test("parsing a multiplication expression produces the correct tree") {
        expression("a * 1") should parseToExp(StarExp(IdnExp(IdnUse("a")), IntExp(1)))
    }

    test("parsing an index expression produces the correct tree") {
        expression("a[1]") should parseToExp(IndExp(IdnExp(IdnUse("a")), IntExp(1)))
    }

    test("parsing a length expression produces the correct tree") {
        expression("a.length") should parseToExp(LengthExp(IdnExp(IdnUse("a"))))
    }

    test("parsing a method call expression produces the correct tree (no args)") {
        expression("a.m ()") should parseToExp(
            CallExp(IdnExp(IdnUse("a")), IdnUse("m"), Vector())
        )
    }

    test("parsing a method call expression produces the correct tree (one arg)") {
        expression("a.m (1)") should parseToExp(
            CallExp(IdnExp(IdnUse("a")), IdnUse("m"), Vector(IntExp(1)))
        )
    }

    test("parsing a method call expression produces the correct tree (many args)") {
        expression("a.m (1, 2, 3)") should parseToExp(
            CallExp(IdnExp(IdnUse("a")), IdnUse("m"), Vector(IntExp(1), IntExp(2), IntExp(3)))
        )
    }

    test("parsing an integer expression produces the correct tree") {
        expression("823") should parseToExp(IntExp(823))
    }

    test("parsing a true expression produces the correct tree") {
        expression("true") should parseToExp(TrueExp())
    }

    test("parsing a false expression produces the correct tree") {
        expression("false") should parseToExp(FalseExp())
    }

    test("parsing a this expression produces the correct tree") {
        expression("this") should parseToExp(ThisExp())
    }

    test("parsing an identifier expression produces the correct tree") {
        expression("v123") should parseToExp(IdnExp(IdnUse("v123")))
    }

    test("parsing a new array expression produces the correct tree") {
        expression("new int [42]") should parseToExp(NewArrayExp(IntExp(42)))
    }

    test("parsing a new expression produces the correct tree") {
        expression("new Foo ()") should parseToExp(NewExp(IdnUse("Foo")))
    }

    test("parsing a not expression produces the correct tree") {
        expression("!a") should parseToExp(NotExp(IdnExp(IdnUse("a"))))
    }

    test("parsing a parenthesized expression produces the correct tree") {
        expression("(a + 5)") should parseToExp(
            PlusExp(IdnExp(IdnUse("a")), IntExp(5))
        )
    }

    test("two identifiers in a row are not a legal expression") {
        expression("a b") should parseToExp(IdnExp(IdnUse("a")))
    }

    // Tests of binary operator associativity

    test("&& is left associative") {
        expression("a && b && c") should parseToExp(
            AndExp(AndExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("+ is left associative") {
        expression("a + b + c") should parseToExp(
            PlusExp(PlusExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("- is left associative") {
        expression("a - b - c") should parseToExp(
            MinusExp(MinusExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("* is left associative") {
        expression("a * b * c") should parseToExp(
            StarExp(StarExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("< is not associative") {
        expression("a < b < c") should parseToExp(LessExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))))
    }

    test("parentheses override associativity") {
        expression("a + (b + c)") should parseToExp(
            PlusExp(IdnExp(IdnUse("a")), PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))))
        )
    }

    // Tests of binary operator relative precedence

    test("&& has lower precedence than < (to left)") {
        expression("a && b < c") should parseToExp(
            AndExp(IdnExp(IdnUse("a")), LessExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))))
        )
    }

    test("< has lower precedence than + (to left)") {
        expression("a < b + c") should parseToExp(
            LessExp(IdnExp(IdnUse("a")), PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))))
        )
    }

    test("< has lower precedence than - (to left)") {
        expression("a < b - c") should parseToExp(
            LessExp(IdnExp(IdnUse("a")), MinusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))))
        )
    }

    test("+ has lower precedence than * (to left)") {
        expression("a + b * c") should parseToExp(
            PlusExp(IdnExp(IdnUse("a")), StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))))
        )
    }

    test("- has lower precedence than * (to left)") {
        expression("a - b * c") should parseToExp(
            MinusExp(IdnExp(IdnUse("a")), StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))))
        )
    }

    test("&& has lower precedence than < (to right)") {
        expression("a < b && c") should parseToExp(
            AndExp(LessExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("< has lower precedence than + (to right)") {
        expression("a + b < c") should parseToExp(
            LessExp(PlusExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("< has lower precedence than - (to right)") {
        expression("a - b < c") should parseToExp(
            LessExp(MinusExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("+ has lower precedence than * (to right)") {
        expression("a * b + c") should parseToExp(
            PlusExp(StarExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("- has lower precedence than * (to right)") {
        expression("a * b - c") should parseToExp(
            MinusExp(StarExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("parentheses override precedence (to left)") {
        expression("(a + b) * c") should parseToExp(
            StarExp(PlusExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c")))
        )
    }

    test("parentheses override precedence (to right)") {
        expression("a * (b + c)") should parseToExp(
            StarExp(IdnExp(IdnUse("a")), PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))))
        )
    }

    // Tests of parsing statements

    test("parsing a block produces the correct tree (no statement)") {
        statement("{ }") should parseToStmt(Block(Vector()))
    }

    test("parsing a block produces the correct tree (one statement)") {
        statement("{ a = 1; }") should parseToStmt(
            Block(Vector(
                VarAssign(IdnUse("a"), IntExp(1))
            ))
        )
    }

    test("parsing a block produces the correct tree (many statements)") {
        statement("{ a = 1; b = 2; c = 3; }") should parseToStmt(
            Block(Vector(
                VarAssign(IdnUse("a"), IntExp(1)),
                VarAssign(IdnUse("b"), IntExp(2)),
                VarAssign(IdnUse("c"), IntExp(3))
            ))
        )
    }

    test("parsing an assignment statement produces the correct tree") {
        statement("a = 1;") should parseToStmt(VarAssign(IdnUse("a"), IntExp(1)))
    }

    test("parsing an if statement produces the correct tree") {
        statement("if (a < 1) b = 1; else c = 1;") should parseToStmt(
            If(
                LessExp(IdnExp(IdnUse("a")), IntExp(1)),
                VarAssign(IdnUse("b"), IntExp(1)),
                VarAssign(IdnUse("c"), IntExp(1))
            )
        )
    }

    test("parsing a while statement produces the correct tree") {
        statement("while (a < 1) b = 1;") should parseToStmt(
            While(
                LessExp(IdnExp(IdnUse("a")), IntExp(1)),
                VarAssign(IdnUse("b"), IntExp(1))
            )
        )

    }

    test("parsing a println statement produces the correct tree") {
        statement("System.out.println (foo);") should parseToStmt(
            Println(IdnExp(IdnUse("foo")))
        )
    }

    test("parsing a variable assignment statement produces the correct tree") {
        statement("a = 1;") should parseToStmt(
            VarAssign(IdnUse("a"), IntExp(1))
        )
    }

    test("parsing an array assignment statement produces the correct tree") {
        statement("a[1] = 2;") should parseToStmt(
            ArrayAssign(IdnUse("a"), IntExp(1), IntExp(2))
        )
    }

    // Program tests

    test("a program with just a main class parses") {
        program(
            """
            class Main { public static void main () { a = 1; } }
            """
        ) should parseTo(
                Program(
                    MainClass(IdnDef("Main"), MainMethod(VarAssign(IdnUse("a"), IntExp(1)))),
                    Vector()
                )
            )
    }

    test("a program with a main class and one normal class parses") {
        program(
            """class Main { public static void main () { a = 1; } }
            class Normal1 { }"""
        ) should parseTo(
                Program(
                    MainClass(IdnDef("Main"), MainMethod(VarAssign(IdnUse("a"), IntExp(1)))),
                    Vector(
                        Class(IdnDef("Normal1"), None, ClassBody(Vector(), Vector()))
                    )
                )
            )
    }

    test("a program with a main class and two normal classes parses") {
        program(
            """class Main { public static void main () { a = 1; } }
            class Normal1 { }
            class Normal2 { }"""
        ) should parseTo(
                Program(
                    MainClass(IdnDef("Main"), MainMethod(VarAssign(IdnUse("a"), IntExp(1)))),
                    Vector(
                        Class(IdnDef("Normal1"), None, ClassBody(Vector(), Vector())),
                        Class(IdnDef("Normal2"), None, ClassBody(Vector(), Vector()))
                    )
                )
            )
    }

    // Main declaration tests

    test("a main class parses correctly") {
        mainClass(
            "class Bob { public static void main () { a = 1; } }"
        ) should parseTo(
                MainClass(IdnDef("Bob"), MainMethod(VarAssign(IdnUse("a"), IntExp(1))))
            )
    }

    test("a main class with more than one statement doesn't parse") {
        mainClass(
            "class Bob { public static void main () { a = 1; a = 2; } }"
        ) should failParseAt(1, 49, """'}' expected but 'a' found""")
    }

    // Class declaration tests

    test("a class with no extends clause, empty body parses") {
        classDeclaration("class Foo { }") should parseTo(
            Class(IdnDef("Foo"), None, ClassBody(Vector(), Vector()))
        )
    }

    test("a class with an extends clause, empty body parses") {
        classDeclaration("class Foo extends Bar { }") should parseTo(
            Class(IdnDef("Foo"), Some(IdnUse("Bar")), ClassBody(Vector(), Vector()))
        )
    }

    test("a class with just a var declaration parses") {
        classDeclaration("class Foo { int a; }") should parseTo(
            Class(IdnDef("Foo"), None,
                ClassBody(
                    Vector(Field(IntType(), IdnDef("a"))),
                    Vector()
                ))
        )
    }

    test("a class with many var declarations parses") {
        classDeclaration(
            "class Foo { int a; int b; int c; }"
        ) should parseTo(
                Class(IdnDef("Foo"), None,
                    ClassBody(
                        Vector(
                            Field(IntType(), IdnDef("a")),
                            Field(IntType(), IdnDef("b")),
                            Field(IntType(), IdnDef("c"))
                        ),
                        Vector()
                    ))
            )
    }

    test("a class with just one method parses") {
        classDeclaration(
            """
            class Foo {
                public int m1 () { return 1; }
            }"""
        ) should parseTo(
                Class(IdnDef("Foo"), None,
                    ClassBody(
                        Vector(),
                        Vector(Method(
                            IdnDef("m1"),
                            MethodBody(
                                IntType(),
                                Vector(),
                                Vector(),
                                Vector(),
                                Result(IntExp(1))
                            )
                        ))
                    ))
            )
    }

    test("a class with many methods parses") {
        classDeclaration(
            """
            class Foo {
                public int m1 () { return 1; }
                public int m2 () { return 2; }
                public int m3 () { return 3; }
            }"""
        ) should parseTo(
                Class(IdnDef("Foo"), None,
                    ClassBody(
                        Vector(),
                        Vector(
                            Method(
                                IdnDef("m1"),
                                MethodBody(
                                    IntType(),
                                    Vector(),
                                    Vector(),
                                    Vector(),
                                    Result(IntExp(1))
                                )
                            ),
                            Method(
                                IdnDef("m2"),
                                MethodBody(
                                    IntType(),
                                    Vector(),
                                    Vector(),
                                    Vector(),
                                    Result(IntExp(2))
                                )
                            ),
                            Method(
                                IdnDef("m3"),
                                MethodBody(
                                    IntType(),
                                    Vector(),
                                    Vector(),
                                    Vector(),
                                    Result(IntExp(3))
                                )
                            )
                        )
                    ))
            )
    }

    test("a class with many variable declarations and many methods parses") {
        classDeclaration(
            """
            class Foo {
                int a; int b; int c;
                public int m1 () { return 1; }
                public int m2 () { return 2; }
                public int m3 () { return 3; }
            }"""
        ) should parseTo(
                Class(IdnDef("Foo"), None,
                    ClassBody(
                        Vector(
                            Field(IntType(), IdnDef("a")),
                            Field(IntType(), IdnDef("b")),
                            Field(IntType(), IdnDef("c"))
                        ),
                        Vector(
                            Method(
                                IdnDef("m1"),
                                MethodBody(
                                    IntType(),
                                    Vector(),
                                    Vector(),
                                    Vector(),
                                    Result(IntExp(1))
                                )
                            ),
                            Method(
                                IdnDef("m2"),
                                MethodBody(
                                    IntType(),
                                    Vector(),
                                    Vector(),
                                    Vector(),
                                    Result(IntExp(2))
                                )
                            ),
                            Method(
                                IdnDef("m3"),
                                MethodBody(
                                    IntType(),
                                    Vector(),
                                    Vector(),
                                    Vector(),
                                    Result(IntExp(3))
                                )
                            )
                        )
                    ))
            )
    }

    // Variable declaration and type tests

    test("an integer variable declaration produces the correct tree") {
        varDeclaration("int a;") should parseTo(
            Var(IntType(), IdnDef("a"))
        )
    }

    test("a Boolean variable declaration produces the correct tree") {
        varDeclaration("boolean a;") should parseTo(
            Var(BooleanType(), IdnDef("a"))
        )
    }

    test("an integer array variable declaration produces the correct tree") {
        varDeclaration("int[] a;") should parseTo(
            Var(IntArrayType(), IdnDef("a"))
        )
    }

    test("a class variable declaration produces the correct tree") {
        varDeclaration("Foo a;") should parseTo(
            Var(ClassType(IdnUse("Foo")), IdnDef("a"))
        )
    }

    // Method declaration tests

    test("a method with an empty body does not parse") {
        methodDeclaration("public int m () { }") should failParseAt(1, 19,
            """'return' expected but '}' found""")
    }

    test("a method with just a return statement parses") {
        methodDeclaration(
            "public int m () { return 1; }"
        ) should parseTo(
                Method(
                    IdnDef("m"),
                    MethodBody(
                        IntType(),
                        Vector(),
                        Vector(),
                        Vector(),
                        Result(IntExp(1))
                    )
                )
            )
    }

    test("a method with one variable declaration parses") {
        methodDeclaration(
            "public int m () { int a; return 1; }"
        ) should parseTo(
                Method(
                    IdnDef("m"),
                    MethodBody(
                        IntType(),
                        Vector(),
                        Vector(Var(IntType(), IdnDef("a"))),
                        Vector(),
                        Result(IntExp(1))
                    )
                )
            )
    }

    test("a method with one statement parses") {
        methodDeclaration(
            "public int m () { a = 1; return 1; }"
        ) should parseTo(
                Method(
                    IdnDef("m"),
                    MethodBody(
                        IntType(),
                        Vector(),
                        Vector(),
                        Vector(VarAssign(IdnUse("a"), IntExp(1))),
                        Result(IntExp(1))
                    )
                )
            )
    }

    test("a method with mutliple variables and statements parses") {
        methodDeclaration(
            "public int m () { int a; int b; a = 1; b = 1; return 1; }"
        ) should parseTo(
                Method(
                    IdnDef("m"),
                    MethodBody(
                        IntType(),
                        Vector(),
                        Vector(
                            Var(IntType(), IdnDef("a")),
                            Var(IntType(), IdnDef("b"))
                        ),
                        Vector(
                            VarAssign(IdnUse("a"), IntExp(1)),
                            VarAssign(IdnUse("b"), IntExp(1))
                        ),
                        Result(IntExp(1))
                    )
                )
            )
    }

    test("a method with no arguments parses") {
        methodDeclaration(
            "public int m () { return 1; }"
        ) should parseTo(
                Method(
                    IdnDef("m"),
                    MethodBody(
                        IntType(),
                        Vector(),
                        Vector(),
                        Vector(),
                        Result(IntExp(1))
                    )
                )
            )
    }

    test("a method with one argument parses") {
        methodDeclaration(
            "public int m (int a) { return 1; }"
        ) should parseTo(
                Method(
                    IdnDef("m"),
                    MethodBody(
                        IntType(),
                        Vector(Argument(IntType(), IdnDef("a"))),
                        Vector(),
                        Vector(),
                        Result(IntExp(1))
                    )
                )
            )
    }

    test("a method with many arguments parses") {
        methodDeclaration(
            "public int m (int a, int b, int c) { return 1; }"
        ) should parseTo(
                Method(
                    IdnDef("m"),
                    MethodBody(
                        IntType(),
                        Vector(
                            Argument(IntType(), IdnDef("a")),
                            Argument(IntType(), IdnDef("b")),
                            Argument(IntType(), IdnDef("c"))
                        ),
                        Vector(),
                        Vector(),
                        Result(IntExp(1))
                    )
                )
            )
    }

    test("an empty argument list parses") {
        arguments("") should parseTo(Vector[Argument]())
    }

    test("a singleton argument list parses") {
        arguments("int a") should parseTo(
            Vector(Argument(IntType(), IdnDef("a")))
        )
    }

    test("an argument list with many arguments parses") {
        arguments("int a, int b, int c") should parseTo(
            Vector(
                Argument(IntType(), IdnDef("a")),
                Argument(IntType(), IdnDef("b")),
                Argument(IntType(), IdnDef("c"))
            )
        )
    }

    // Input text recovery tests

    test("a standalone node has no input text") {
        val t = Argument(IntType(), IdnDef("a"))
        positions.textOf(t) shouldBe empty
    }

    test("input text from an identifier parse is the identifier") {
        expression("    x") should parseText("x")
    }

    test("input text from an operator expression parse is correct") {
        expression("  x   +  1 ") should parseText("x   +  1")
    }

    test("input text from a statement with comments is correct") {
        statement("""
            |// In
            |while (a < 1) { // In
            |    b = 1;
            |    // In
            |}
            |// Out
            |""".stripMargin) should parseText(
            """while (a < 1) { // In
            |    b = 1;
            |    // In
            |}""".stripMargin
        )
    }

}
