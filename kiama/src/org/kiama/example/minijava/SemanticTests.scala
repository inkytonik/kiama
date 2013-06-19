/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2013 Anthony M Sloane, Macquarie University.
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

import org.scalatest.FunSuite

/**
 * Parser to use for semantic tests. Separated from `SemanticTests` since
 * we only need the program parser and we don't want to bring all of the
 * parsers into scope in the tests.
 */
object SemanticTestParser extends SyntaxAnalysis

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
class SemanticTests extends FunSuite {

    import MiniJavaTree._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.Messaging.{messagecount, messages, resetmessages}
    import SemanticAnalysis._
    import SemanticTestParser.{Error, parser, parseAll, Success, Failure}

    // Tests of definition uniqueness (Rule 1)

    test ("two declarations of same class is an error") {
        parseTest ("""
            |class Main { public static void main () { System.out.println (0); } }
            |class Main { }
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 7, "Main is declared more than once")
        assertMessage (1, 3, 7, "Main is declared more than once")
    }

    test ("two declarations of same name in same class is an error") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int mult;
            |    int mult;
            |}
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 4, 9, "mult is declared more than once")
        assertMessage (1, 5, 9, "mult is declared more than once")
    }

    test ("two declarations of same name in different scopes is ok") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int notmult;
            |    public int m () {
            |        int notmult;
            |        return 0;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    // Test of applied occurence matching defining occurrence (Rule 2)

    test ("use of a name that is not declared is an error") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        notdecl = 1;
            |        return 0;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 5, 9, "notdecl is not declared")
    }

    test ("use of a name that is declared in wrong scope is an error") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m1 () {
            |        int notdecl;
            |        return 0;
            |    }
            |    public int m2 () {
            |        notdecl = 1;
            |        return 0;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 9, 9, "notdecl is not declared")
    }

    // Test type of integer expression (Rule 4)

    test ("an integer expression has integer type") {
        val exp = IntExp (42)
        embedExpressionAndCheck (exp)
        expectResult (IntType ()) (exp->tipe)
    }

    // Test type of boolean expressions (Rule 5)

    test ("a true expression has Boolean type") {
        val exp = TrueExp ()
        embedExpressionAndCheck (exp)
        expectResult (BooleanType ()) (exp->tipe)
    }

    test ("a false expression has Boolean type") {
        val exp = FalseExp ()
        embedExpressionAndCheck (exp)
        expectResult (BooleanType ()) (exp->tipe)
    }

    // Test use of method names in expressions (rule 6)

    test ("a method name cannot be used in an expression") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return m;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 6, 16, "can't refer to methods directly")
    }

    // Test type of condition in if and while statements (Rule 7)

    test ("the condition of an if statement can have Boolean type") {
        val exp = IntExp (0) // dummy
        val cond = TrueExp ()
        val stmts = List (If (cond, Block (Nil), Block (Nil)))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 0)
    }

    test ("the condition of an if statement cannot have integer type") {
        val exp = IntExp (0) // dummy
        val cond = IntExp (42)
        val stmts = List (If (cond, Block (Nil), Block (Nil)))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected boolean got int")
    }

    test ("the condition of a while statement can have Boolean type") {
        val exp = IntExp (0) // dummy
        val cond = TrueExp ()
        val stmts = List (While (cond, Block (Nil)))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 0)
    }

    test ("the condition of a while statement cannot have integer type") {
        val exp = IntExp (0) // dummy
        val cond = IntExp (42)
        val stmts = List (While (cond, Block (Nil)))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected boolean got int")
    }

    // Test type of expression in println statement can be of any type (Rule 8)

    test ("the expression in a println statement can be of Boolean type") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val stmts = List (Println (exp1))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 0)
    }

    test ("the expression in a println statement can be of integer type") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val stmts = List (Println (exp1))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 0)
    }

    test ("the expression in a println statement can be of integer array type") {
        val exp = IntExp (0) // dummy
        val exp1 = NewArrayExp (IntExp (42))
        val stmts = List (Println (exp1))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 0)
    }

    test ("the expression in a println statement can be of reference type") {
        val exp = IntExp (0) // dummy
        val exp1 = NewExp (IdnUse ("Test"))
        val stmts = List (Println (exp1))
        embedExpressionAndCheck (exp, IntType (), Nil, stmts)
        assert (messagecount === 0)
    }

    // Test that assignment RHSes have compatible types with LHS (Rule 9)

    test ("an integer expression is assignment compatible with an integer var") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val vars = List (Var (IntType (), IdnDef ("v")))
        val stmts = List (VarAssign (IdnUse ("v"), exp1))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 0)
    }

    test ("a Boolean expression is not assignment compatible with an integer var") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val vars = List (Var (IntType (), IdnDef ("v")))
        val stmts = List (VarAssign (IdnUse ("v"), exp1))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected int got boolean")
    }

    test ("a Boolean expression is assignment compatible with a Boolean var") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val vars = List (Var (BooleanType (), IdnDef ("v")))
        val stmts = List (VarAssign (IdnUse ("v"), exp1))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 0)
    }

    test ("an integer expression is not assignment compatible with a Boolean var") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val vars = List (Var (BooleanType (), IdnDef ("v")))
        val stmts = List (VarAssign (IdnUse ("v"), exp1))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected boolean got int")
    }

    test ("an integer array expression is assignment compatible with an integer array var") {
        val exp = IntExp (0) // dummy
        val exp1 = NewArrayExp (IntExp (42))
        val vars = List (Var (IntArrayType (), IdnDef ("v")))
        val stmts = List (VarAssign (IdnUse ("v"), exp1))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 0)
    }

    test ("an integer expression is not assignment compatible with an integer array var") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val vars = List (Var (IntArrayType (), IdnDef ("v")))
        val stmts = List (VarAssign (IdnUse ("v"), exp1))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected int[] got int")
    }

    // Test types in array assignments (Rule 10)

    test ("integer expressions are ok in an integer array assignment") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val exp2 = IntExp (99)
        val vars = List (Var (IntArrayType (), IdnDef ("v")))
        val stmts = List (ArrayAssign (IdnUse ("v"), exp1, exp2))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 0)
    }

    test ("Boolean expressions are not ok in an integer array assignment") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val exp2 = FalseExp ()
        val vars = List (Var (IntArrayType (), IdnDef ("v")))
        val stmts = List (ArrayAssign (IdnUse ("v"), exp1, exp2))
        embedExpressionAndCheck (exp, IntType (), vars, stmts)
        assert (messagecount === 2)
        assertMessage (0, 0, 0, "type error: expected int got boolean")
        assertMessage (1, 0, 0, "type error: expected int got boolean")
    }

    // Test type of plus expressions (Rule 11)

    test ("the children of a plus expression are allowed to be integers") {
        val exp = PlusExp (IntExp (42), IntExp (99))
        embedExpressionAndCheck (exp)
        assert (messagecount === 0)
    }

    test ("the children of a plus expression must be integers and its type is integer") {
        val exp = PlusExp (TrueExp (), FalseExp ())
        embedExpressionAndCheck (exp)
        assert (messagecount === 2)
        assertMessage (0, 0, 0, "type error: expected int got boolean")
        assertMessage (1, 0, 0, "type error: expected int got boolean")
        expectResult (IntType ()) (exp->tipe)
    }

    // Test type of and expressions (Rule 12)

    test ("the children of an and expression are allowed to be Booleans") {
        val exp = AndExp (TrueExp (), FalseExp ())
        embedExpressionAndCheck (exp, BooleanType ())
        assert (messagecount === 0)
    }

    test ("the children of an and expression must be Booelans and its type is Boolean") {
        val exp = AndExp (IntExp (42), IntExp (99))
        embedExpressionAndCheck (exp, BooleanType ())
        assert (messagecount === 2)
        assertMessage (0, 0, 0, "type error: expected boolean got int")
        assertMessage (1, 0, 0, "type error: expected boolean got int")
        expectResult (BooleanType ()) (exp->tipe)
    }

    // Test type of plus expressions (Rule 13)

    test ("the child of a not expression is allowed to be Boolean") {
        val exp = NotExp (TrueExp ())
        embedExpressionAndCheck (exp, BooleanType ())
        assert (messagecount === 0)
    }

    test ("the child of a not expression must be Boolean and its type is Boolean") {
        val exp = NotExp (IntExp (42))
        embedExpressionAndCheck (exp, BooleanType ())
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected boolean got int")
        expectResult (BooleanType ()) (exp->tipe)
    }

    // Test type of less-than expressions (Rule 14)

    test ("the children of a less-than expression are allowed to be integers") {
        val exp = LessExp (IntExp (42), IntExp (99))
        embedExpressionAndCheck (exp, BooleanType ())
        assert (messagecount === 0)
    }

    test ("the children of a less-than expression must be integers and its type is Boolean") {
        val exp = LessExp (TrueExp (), FalseExp ())
        embedExpressionAndCheck (exp, BooleanType ())
        assert (messagecount === 2)
        assertMessage (0, 0, 0, "type error: expected int got boolean")
        assertMessage (1, 0, 0, "type error: expected int got boolean")
        expectResult (BooleanType ()) (exp->tipe)
    }

    // Test type of length expressions (Rule 15)

    test ("the child of a length expression is allowed to be an integer array") {
        val exp = LengthExp (NewArrayExp (IntExp (42)))
        embedExpressionAndCheck (exp)
        assert (messagecount === 0)
    }

    test ("the child of a length expression must be an integer array and its type is integer") {
        val exp = LengthExp (IntExp (42))
        embedExpressionAndCheck (exp)
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected int[] got int")
        expectResult (IntType ()) (exp->tipe)
    }

    // Test method call expressions (rule 3, 16)

    test ("a non-method cannot be called") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return this.v ();
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 6, 21, "illegal call to non-method")
    }

    test ("a superclass method can be called") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Super {
            |    public int m (int v) {
            |        return v + 1;
            |    }
            |}
            |class Test extends Super {
            |    int v;
            |    public int n () {
            |        return this.m (99);
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("the type of a method call expression is the method return type (1)") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return 42;
            |    }
            |    public int n () {
            |        v = this.m ();
            |        return 0;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("the type of a method call expression is the method return type (2)") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public boolean m () {
            |        return true;
            |    }
            |    public int n () {
            |        v = this.m ();
            |        return 0;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 9, 13, "type error: expected int got boolean")
    }

    test ("the numbers of arguments in a call can match the declaration") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m (int a, int b) {
            |        return 33;
            |    }
            |    public int n () {
            |        return this.m (42, 99);
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("the numbers of arguments in a call must match the declaration") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m (int a, int b) {
            |        return 33;
            |    }
            |    public int n () {
            |        return this.m (42);
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 8, 21, "wrong number of arguments, got 1 but expected 2")
    }

    test ("the types of arguments in a call must match the declaration") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m (boolean a, int[] b) {
            |        return 33;
            |    }
            |    public int n () {
            |        return this.m (42, 99);
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 8, 24, "type error: expected boolean got int")
        assertMessage (1, 8, 28, "type error: expected int[] got int")
    }

    test ("forward references to methods work") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int n () {
            |        v = this.m ();
            |        return 0;
            |    }
            |    public int m () {
            |        return 42;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    // Test the type of "this" (rule 17)

    test ("the type of this is the current class") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return this;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    // Test the types in new integer array expressions (rule 18)

    test ("The type of a new array expression is an integer array") {
        val exp = NewArrayExp (IntExp (42))
        embedExpressionAndCheck (exp, IntArrayType ())
        assert (messagecount === 0)
        expectResult (IntArrayType ()) (exp->tipe)
    }

    test ("The type of the parameter in a new integer array expression must be an integer") {
        val exp = NewArrayExp (TrueExp ())
        embedExpressionAndCheck (exp, IntArrayType ())
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "type error: expected int got boolean")
    }

    // Test the use of names in new expressions (rule 19)

    test ("The name used in a new expression must refer to a class") {
        val exp = NewExp (IdnUse ("v"))
        val vars = List (Var (IntType (), IdnDef ("v")))
        embedExpressionAndCheck (exp, IntType (), vars)
        assert (messagecount === 1)
        assertMessage (0, 0, 0, "illegal instance creation of non-class type")
    }

    test ("The type of a new expression is a reference to the created class") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return (new Test ());
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    // Test the return type of a method (rule 20)

    test ("The return expression of a method can return the appropriate type") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return 42;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("The return expression of a method cannot return an inappropriate type") {
        parseTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return true;
            |    }
            |}
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 5, 16, "type error: expected int got boolean")
    }

    /**
     * Parse some test input as a program and, if the parse succeeds with
     * no input left, return the program tree. If the parse fails, fail
     * the test.
     */
    def parseProgram (str : String) : Program =
        parseAll (parser, str) match {
            case Success (r, in) =>
                if (!in.atEnd) fail (s"input remaining at ${in.pos}")
                r
            case f : Error =>
                fail (s"parse error: $f")
            case f : Failure =>
                fail (s"parse failure: $f")
        }

    /**
     * Parse some test input as a program and run the semantic analyser
     * over the resulting tree (if the parse succeeds).
     */
    def parseTest (str : String) {
        runSemanticChecks (parseProgram (str))
    }

    /**
     * Run the semantic checks on the given program.
     */
    def runSemanticChecks (prog : Program) {
        initTree (prog)
        resetmessages
        check (prog)
    }

    /**
     * Construct a program by inserting the given expression into a return
     * statement of a method and then run the semantic checks on it. The
     * idea is that you construct the expression outside, insert it into
     * the program, run the checks and then you can check that attributes
     * of the expression are as you expect. The optional `retType`, `vars`
     * and `stmts` arguments can be used to inject a return type, variable
     * declarations or statements into the method as well. The return type
     * defaults to integer and the variable and statement lists to empty.
     */
    def embedExpressionAndCheck (exp : Expression,
                                 retType : Type = IntType (),
                                 vars : List[Var] = Nil,
                                 stmts : List[Statement] = Nil) {
        val prog =
            Program (MainClass (IdnDef ("Dummy"), Println (IntExp (0))),
                List(
                    Class (IdnDef ("Test"), None,
                        ClassBody (
                            Nil,
                            List (
                                Method (IdnDef ("m"),
                                    MethodBody (
                                        retType,
                                        Nil,
                                        vars,
                                        stmts,
                                        exp)))))))
        runSemanticChecks (prog)
    }

    /**
     * Assert that a message was produced at a given position.
     */
    def assertMessage (index : Int, line : Int, column : Int, msg : String) {
        val m = messages (index)
        expectResult (msg, s"wrong text in message $index") (m.message)
        expectResult (line, s"wrong line number in message $index") (m.pos.line)
        expectResult (column, s"wrong column number in message $index") (m.pos.column)
    }

}
