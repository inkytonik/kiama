/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2015 Anthony M Sloane, Macquarie University.
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
package example.minijava

import org.kiama.util.RegexParserTests

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
class SyntaxAnalyserTests extends SyntaxAnalyser with RegexParserTests {

    import MiniJavaTree._
    import scala.collection.immutable.Seq

    // Tests of parsing terminals

    test ("parsing an identifier of one letter works") {
        assertParseOk ("x", identifier, "x")
    }

    test ("parsing an identifier as an identifier works") {
        assertParseOk ("count", identifier, "count")
    }

    test ("parsing an identifier containing digits and underscores works") {
        assertParseOk ("x1_2_3", identifier, "x1_2_3")
    }

    test ("parsing an integer as an identifier gives an error") {
        assertParseError ("42", identifier, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9_]*' expected but `4' found")
    }

    test ("parsing a non-identifier as an identifier gives an error (digit)") {
        assertParseError ("4foo", identifier, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9_]*' expected but `4' found")
    }

    test ("parsing a non-identifier as an identifier gives an error (underscore)") {
        assertParseError ("_f3", identifier, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9_]*' expected but `_' found")
    }

    test ("parsing an integer of one digit as an integer works") {
        assertParseOk ("8", integer, "8")
    }

    test ("parsing an integer as an integer works") {
        assertParseOk ("99", integer, "99")
    }

    test ("parsing a non-integer as an integer gives an error") {
        assertParseError ("total", integer, 1, 1,
            "string matching regex `[0-9]+' expected but `t' found")
    }

    // Tests of parsing basic expressions

    test ("parsing an and expression produces the correct tree") {
        assertParseOk ("a && 1", expression, AndExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a less then expression produces the correct tree") {
        assertParseOk ("a < 1", expression, LessExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing an addition expression produces the correct tree") {
        assertParseOk ("a + 1", expression, PlusExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a subtraction expression produces the correct tree") {
        assertParseOk ("a - 1", expression, MinusExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a multiplication expression produces the correct tree") {
        assertParseOk ("a * 1", expression, StarExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing an index expression produces the correct tree") {
        assertParseOk ("a[1]", expression, IndExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a length expression produces the correct tree") {
        assertParseOk ("a.length", expression, LengthExp (IdnExp (IdnUse ("a"))))
    }

    test ("parsing a method call expression produces the correct tree (no args)") {
        assertParseOk ("a.m ()", expression,
            CallExp (IdnExp (IdnUse ("a")), IdnUse ("m"), Nil))
    }

    test ("parsing a method call expression produces the correct tree (one arg)") {
        assertParseOk ("a.m (1)", expression,
            CallExp (IdnExp (IdnUse ("a")), IdnUse ("m"), Seq (IntExp (1))))
    }

    test ("parsing a method call expression produces the correct tree (many args)") {
        assertParseOk ("a.m (1, 2, 3)", expression,
            CallExp (IdnExp (IdnUse ("a")), IdnUse ("m"), Seq (IntExp (1), IntExp (2), IntExp (3))))
    }

    test ("parsing an integer expression produces the correct tree") {
        assertParseOk ("823", expression, IntExp (823))
    }

    test ("parsing a true expression produces the correct tree") {
        assertParseOk ("true", expression, TrueExp ())
    }

    test ("parsing a false expression produces the correct tree") {
        assertParseOk ("false", expression, FalseExp ())
    }

    test ("parsing a this expression produces the correct tree") {
        assertParseOk ("this", expression, ThisExp ())
    }

    test ("parsing an identifier expression produces the correct tree") {
        assertParseOk ("v123", expression, IdnExp (IdnUse ("v123")))
    }

    test ("parsing a new array expression produces the correct tree") {
        assertParseOk ("new int [42]", expression, NewArrayExp (IntExp (42)))
    }

    test ("parsing a new expression produces the correct tree") {
        assertParseOk ("new Foo ()", expression, NewExp (IdnUse ("Foo")))
    }

    test ("parsing a not expression produces the correct tree") {
        assertParseOk ("!a", expression, NotExp (IdnExp (IdnUse ("a"))))
    }

    test ("parsing a parenthesized expression produces the correct tree") {
        assertParseOk ("(a + 5)", expression,
            PlusExp (IdnExp (IdnUse ("a")), IntExp (5)))
    }

    test ("two identifiers in a row are not a legal expression") {
        assertParseError ("a b", expression, 1, 3,
            """string matching regex `\z' expected but `b' found""")
    }

    // Tests of binary operator associativity

    test ("&& is left associative") {
        assertParseOk ("a && b && c", expression,
            AndExp (AndExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("+ is left associative") {
        assertParseOk ("a + b + c", expression,
            PlusExp (PlusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("- is left associative") {
        assertParseOk ("a - b - c", expression,
            MinusExp (MinusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("* is left associative") {
        assertParseOk ("a * b * c", expression,
            StarExp (StarExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("< is not associative") {
        assertParseError ("a < b < c", expression, 1, 7,
            """string matching regex `\z' expected but `<' found""")
    }

    test ("parentheses override associativity") {
        assertParseOk ("a + (b + c)", expression,
            PlusExp (IdnExp (IdnUse ("a")), PlusExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    // Tests of binary operator relative precedence

    test ("&& has lower precedence than < (to left)") {
        assertParseOk ("a && b < c", expression,
            AndExp (IdnExp (IdnUse ("a")), LessExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("< has lower precedence than + (to left)") {
        assertParseOk ("a < b + c", expression,
            LessExp (IdnExp (IdnUse ("a")), PlusExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("< has lower precedence than - (to left)") {
        assertParseOk ("a < b - c", expression,
            LessExp (IdnExp (IdnUse ("a")), MinusExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("+ has lower precedence than * (to left)") {
        assertParseOk ("a + b * c", expression,
            PlusExp (IdnExp (IdnUse ("a")), StarExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("- has lower precedence than * (to left)") {
        assertParseOk ("a - b * c", expression,
            MinusExp (IdnExp (IdnUse ("a")), StarExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("&& has lower precedence than < (to right)") {
        assertParseOk ("a < b && c", expression,
            AndExp (LessExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("< has lower precedence than + (to right)") {
        assertParseOk ("a + b < c", expression,
            LessExp (PlusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("< has lower precedence than - (to right)") {
        assertParseOk ("a - b < c", expression,
            LessExp (MinusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("+ has lower precedence than * (to right)") {
        assertParseOk ("a * b + c", expression,
            PlusExp (StarExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("- has lower precedence than * (to right)") {
        assertParseOk ("a * b - c", expression,
            MinusExp (StarExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("parentheses override precedence (to left)") {
        assertParseOk ("(a + b) * c", expression,
            StarExp (PlusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("parentheses override precedence (to right)") {
        assertParseOk ("a * (b + c)", expression,
            StarExp (IdnExp (IdnUse ("a")), PlusExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    // Tests of parsing statements

    test ("parsing a block produces the correct tree (no statement)") {
        assertParseOk ("{ }", statement,
            Block (Nil))
    }

    test ("parsing a block produces the correct tree (one statement)") {
        assertParseOk ("{ a = 1; }", statement,
            Block (Seq (
                VarAssign (IdnUse ("a"), IntExp (1)))))
    }

    test ("parsing a block produces the correct tree (many statements)") {
        assertParseOk ("{ a = 1; b = 2; c = 3; }", statement,
            Block (Seq (
                VarAssign (IdnUse ("a"), IntExp (1)),
                VarAssign (IdnUse ("b"), IntExp (2)),
                VarAssign (IdnUse ("c"), IntExp (3)))))
    }

    test ("parsing an assignment statement produces the correct tree") {
        assertParseOk ("a = 1;", statement, VarAssign (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an if statement produces the correct tree") {
        assertParseOk ("if (a < 1) b = 1; else c = 1;", statement,
            If (LessExp (IdnExp (IdnUse ("a")), IntExp (1)),
                VarAssign (IdnUse ("b"), IntExp (1)),
                VarAssign (IdnUse ("c"), IntExp (1))))
    }

    test ("parsing a while statement produces the correct tree") {
        assertParseOk ("while (a < 1) b = 1;", statement,
            While (LessExp (IdnExp (IdnUse ("a")), IntExp (1)),
                   VarAssign (IdnUse ("b"), IntExp (1))))

    }

    test ("parsing a println statement produces the correct tree") {
        assertParseOk ("System.out.println (foo);", statement,
            Println (IdnExp (IdnUse ("foo"))))
    }

    test ("parsing a variable assignment statement produces the correct tree") {
        assertParseOk ("a = 1;", statement,
            VarAssign (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an array assignment statement produces the correct tree") {
        assertParseOk ("a[1] = 2;", statement,
            ArrayAssign (IdnUse ("a"), IntExp (1), IntExp (2)))
    }

    // Program tests

    test ("a program with just a main class parses") {
        assertParseOk ("""
            class Main { public static void main () { a = 1; } }
            """,
            program,
            Program (
                MainClass (IdnDef ("Main"), VarAssign (IdnUse ("a"), IntExp (1))),
                Nil))
    }

    test ("a program with a main class and one normal class parses") {
        assertParseOk ("""
            class Main { public static void main () { a = 1; } }
            class Normal1 { }
            """,
            program,
            Program (
                MainClass (IdnDef ("Main"), VarAssign (IdnUse ("a"), IntExp (1))),
                Seq (
                    Class (IdnDef ("Normal1"), None, ClassBody (Nil, Nil)))))
    }

    test ("a program with a main class and two normal classes parses") {
        assertParseOk ("""
            class Main { public static void main () { a = 1; } }
            class Normal1 { }
            class Normal2 { }
            """,
            program,
            Program (
                MainClass (IdnDef ("Main"), VarAssign (IdnUse ("a"), IntExp (1))),
                Seq (
                    Class (IdnDef ("Normal1"), None, ClassBody (Nil, Nil)),
                    Class (IdnDef ("Normal2"), None, ClassBody (Nil, Nil)))))
    }

    // Main declaration tests

    test ("a main class parses correctly") {
        assertParseOk ("class Bob { public static void main () { a = 1; } }",
            mainClass,
            MainClass (IdnDef ("Bob"), VarAssign (IdnUse ("a"), IntExp (1))))
    }

    test ("a main class with more than one statement doesn't parse") {
        assertParseError (
            "class Bob { public static void main () { a = 1; a = 2; } }",
            mainClass, 1, 49,
            """`}' expected but `a' found""")
    }

    // Class declaration tests

    test ("a class with no extends clause, empty body parses") {
        assertParseOk ("class Foo { }", classDeclaration,
            Class (IdnDef ("Foo"), None, ClassBody (Nil, Nil)))
    }

    test ("a class with an extends clause, empty body parses") {
        assertParseOk ("class Foo extends Bar { }", classDeclaration,
            Class (IdnDef ("Foo"), Some (IdnUse ("Bar")), ClassBody (Nil, Nil)))
    }

    test ("a class with just a var declaration parses") {
        assertParseOk ("class Foo { int a; }", classDeclaration,
            Class (IdnDef ("Foo"), None,
                ClassBody (Seq (Field (IntType (), IdnDef ("a"))),
                           Nil)))
    }

    test ("a class with many var declarations parses") {
        assertParseOk ("class Foo { int a; int b; int c; }",
            classDeclaration,
            Class (IdnDef ("Foo"), None,
                ClassBody (
                    Seq (Field (IntType (), IdnDef ("a")),
                          Field (IntType (), IdnDef ("b")),
                          Field (IntType (), IdnDef ("c"))),
                    Nil)))
    }

    test ("a class with just one method parses") {
        assertParseOk ("""
            class Foo {
                public int m1 () { return 1; }
            }""",
            classDeclaration,
            Class (IdnDef ("Foo"), None,
                ClassBody (
                    Nil,
                    Seq (Method (IdnDef ("m1"),
                            MethodBody (
                                IntType (),
                                Nil,
                                Nil,
                                Nil,
                                IntExp (1)))))))
    }

    test ("a class with many methods parses") {
        assertParseOk ("""
            class Foo {
                public int m1 () { return 1; }
                public int m2 () { return 2; }
                public int m3 () { return 3; }
            }""",
            classDeclaration,
            Class (IdnDef ("Foo"), None,
                ClassBody (
                    Nil,
                    Seq (Method (IdnDef ("m1"),
                              MethodBody (
                                  IntType (),
                                  Nil,
                                  Nil,
                                  Nil,
                                  IntExp (1))),
                          Method (IdnDef ("m2"),
                              MethodBody (
                                  IntType (),
                                  Nil,
                                  Nil,
                                  Nil,
                                  IntExp (2))),
                          Method (IdnDef ("m3"),
                              MethodBody (
                                  IntType (),
                                  Nil,
                                  Nil,
                                  Nil,
                                  IntExp (3)))))))
    }

    test ("a class with many variable declarations and many methods parses") {
        assertParseOk ("""
            class Foo {
                int a; int b; int c;
                public int m1 () { return 1; }
                public int m2 () { return 2; }
                public int m3 () { return 3; }
            }""",
            classDeclaration,
            Class (IdnDef ("Foo"), None,
                ClassBody (
                    Seq (Field (IntType (), IdnDef ("a")),
                          Field (IntType (), IdnDef ("b")),
                          Field (IntType (), IdnDef ("c"))),
                    Seq (Method (IdnDef ("m1"),
                              MethodBody (
                                  IntType (),
                                  Nil,
                                  Nil,
                                  Nil,
                                  IntExp (1))),
                          Method (IdnDef ("m2"),
                              MethodBody (
                                  IntType (),
                                  Nil,
                                  Nil,
                                  Nil,
                                  IntExp (2))),
                          Method (IdnDef ("m3"),
                              MethodBody (
                                  IntType (),
                                  Nil,
                                  Nil,
                                  Nil,
                                  IntExp (3)))))))
    }

    // Variable declaration and type tests

    test ("an integer variable declaration produces the correct tree") {
        assertParseOk ("int a;", varDeclaration,
            Var (IntType (), IdnDef ("a")))
    }

    test ("a Boolean variable declaration produces the correct tree") {
        assertParseOk ("boolean a;", varDeclaration,
            Var (BooleanType (), IdnDef ("a")))
    }

    test ("an integer array variable declaration produces the correct tree") {
        assertParseOk ("int[] a;", varDeclaration,
            Var (IntArrayType (), IdnDef ("a")))
    }

    test ("a class variable declaration produces the correct tree") {
        assertParseOk ("Foo a;", varDeclaration,
            Var (ClassType (IdnUse ("Foo")), IdnDef ("a")))
    }

    // Method declaration tests

    test ("a method with an empty body does not parse") {
        assertParseError ("public int m () { }",
            methodDeclaration, 1, 19,
            """`return' expected but `}' found""")
    }

    test ("a method with just a return statement parses") {
        assertParseOk ("public int m () { return 1; }",
            methodDeclaration,
            Method (IdnDef ("m"),
                MethodBody (
                    IntType (),
                    Nil,
                    Nil,
                    Nil,
                    IntExp (1))))
    }

    test ("a method with one variable declaration parses") {
        assertParseOk ("public int m () { int a; return 1; }",
            methodDeclaration,
            Method (IdnDef ("m"),
                MethodBody (
                    IntType (),
                    Nil,
                    Seq (Var (IntType (), IdnDef ("a"))),
                    Nil,
                    IntExp (1))))
    }

    test ("a method with one statement parses") {
        assertParseOk ("public int m () { a = 1; return 1; }",
            methodDeclaration,
            Method (IdnDef ("m"),
                MethodBody (
                    IntType (),
                    Nil,
                    Nil,
                    Seq (VarAssign (IdnUse ("a"), IntExp (1))),
                    IntExp (1))))
    }

    test ("a method with mutliple variables and statements parses") {
        assertParseOk ("public int m () { int a; int b; a = 1; b = 1; return 1; }",
            methodDeclaration,
            Method (IdnDef ("m"),
                MethodBody (
                    IntType (),
                    Nil,
                    Seq (Var (IntType (), IdnDef ("a")),
                          Var (IntType (), IdnDef ("b"))),
                    Seq (VarAssign (IdnUse ("a"), IntExp (1)),
                          VarAssign (IdnUse ("b"), IntExp (1))),
                    IntExp (1))))
    }

    test ("a method with no arguments parses") {
        assertParseOk ("public int m () { return 1; }",
            methodDeclaration,
            Method (IdnDef ("m"),
                MethodBody (
                    IntType (),
                    Nil,
                    Nil,
                    Nil,
                    IntExp (1))))
    }

    test ("a method with one argument parses") {
        assertParseOk ("public int m (int a) { return 1; }",
            methodDeclaration,
            Method (IdnDef ("m"),
                MethodBody (
                    IntType (),
                    Seq (Argument (IntType (), IdnDef ("a"))),
                    Nil,
                    Nil,
                    IntExp (1))))
    }

    test ("a method with many arguments parses") {
        assertParseOk ("public int m (int a, int b, int c) { return 1; }",
            methodDeclaration,
            Method (IdnDef ("m"),
                MethodBody (
                    IntType (),
                    Seq (Argument (IntType (), IdnDef ("a")),
                          Argument (IntType (), IdnDef ("b")),
                          Argument (IntType (), IdnDef ("c"))),
                    Nil,
                    Nil,
                    IntExp (1))))
    }

    test ("an empty argument list parses") {
        assertParseOk ("", arguments, Nil)
    }

    test ("a singleton argument list parses") {
        assertParseOk ("int a", arguments,
            Seq (Argument (IntType (), IdnDef ("a"))))
    }

    test ("an argument list with many arguments parses") {
        assertParseOk ("int a, int b, int c", arguments,
            Seq (Argument (IntType (), IdnDef ("a")),
                  Argument (IntType (), IdnDef ("b")),
                  Argument (IntType (), IdnDef ("c"))))
    }

    // Input text recovery tests

    test ("a standalone node has no input text") {
        val t = Argument (IntType (), IdnDef ("a"))
        assertResult (None) (textOf (t))
    }

    test ("input text from an identifier parse is the identifier") {
        val t = assertParseReturn ("x", expression)
        assertResult (Some ("x")) (textOf (t))
    }

    test ("input text from an operator expression parse is correct") {
        val t = assertParseReturn ("  x   +  1 ", expression)
        assertResult (Some ("  x   +  1")) (textOf (t))
    }

    test ("input text from a statement with comments is correct") {
        val t = assertParseReturn ("""
                    |// In
                    |while (a < 1) { // In
                    |    b = 1;
                    |    // In
                    |}
                    |// Out
                    |""".stripMargin, statement)
        assertResult (Some ("""
                    |// In
                    |while (a < 1) { // In
                    |    b = 1;
                    |    // In
                    |}""".stripMargin)) (textOf (t))
    }

}
