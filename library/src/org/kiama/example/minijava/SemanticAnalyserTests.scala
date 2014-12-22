/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2014 Anthony M Sloane, Macquarie University.
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
class SemanticAnalyserTests extends SyntaxAnalyser with RegexParserTests {

    import MiniJavaTree._
    import SymbolTable.pretty
    import org.kiama.util.{Message, Messaging}
    import org.kiama.util.Positions.positionAt
    import scala.collection.immutable.Seq

    // Tests of definition uniqueness (Rule 1)

    test ("two declarations of same class is an error") {
        semanticTest ("""
            |class Main { public static void main () { System.out.println (0); } }
            |class Main { }
            """.stripMargin,
            Message ("Main is declared more than once", positionAt (2, 7)),
            Message ("Main is declared more than once", positionAt (3, 7)))
    }

    test ("two declarations of same name in same class is an error") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int mult;
            |    int mult;
            |}
            """.stripMargin,
           Message ("mult is declared more than once", positionAt (4, 9)),
           Message ("mult is declared more than once", positionAt (5, 9)))
    }

    test ("two declarations of same name in different scopes is ok") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int notmult;
            |    public int m () {
            |        int notmult;
            |        return 0;
            |    }
            |}
            """.stripMargin)
    }

    // Test of applied occurence matching defining occurrence (Rule 2)

    test ("use of a name that is not declared is an error") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        notdecl = 1;
            |        return 0;
            |    }
            |}
            """.stripMargin,
            Message ("notdecl is not declared", positionAt (5, 9)))
    }

    test ("use of a name that is declared in wrong scope is an error") {
        semanticTest ("""
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
            """.stripMargin,
            Message ("notdecl is not declared", positionAt (9, 9)))
    }

    // Test type of integer expression (Rule 4)

    test ("an integer expression has integer type") {
        val exp = IntExp (42)
        val analysis = semanticTest (embedExpression (exp))
        assertResult (IntType ()) (analysis.tipe (exp))
    }

    // Test type of boolean expressions (Rule 5)

    test ("a true expression has Boolean type") {
        val exp = TrueExp ()
        val analysis = semanticTest (embedExpression (exp, BooleanType ()))
        assertResult (BooleanType ()) (analysis.tipe (exp))
    }

    test ("a false expression has Boolean type") {
        val exp = FalseExp ()
        val analysis = semanticTest (embedExpression (exp, BooleanType ()))
        assertResult (BooleanType ()) (analysis.tipe (exp))
    }

    // Test use of method names in expressions (rule 6)

    test ("a method name cannot be used in an expression") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return m;
            |    }
            |}
            """.stripMargin,
            Message ("can't refer to methods directly", positionAt (6, 16)))
    }

    // Test type of condition in if and while statements (Rule 7)

    test ("the condition of an if statement can have Boolean type") {
        val exp = IntExp (0) // dummy
        val cond = TrueExp ()
        val stmts = Seq (If (cond, Block (Nil), Block (Nil)))
        semanticTest (embedExpression (exp, IntType (), Nil, stmts))
    }

    test ("the condition of an if statement cannot have integer type") {
        val exp = IntExp (0) // dummy
        val cond = IntExp (42)
        val stmts = Seq (If (cond, Block (Nil), Block (Nil)))
        semanticTest (
            embedExpression (exp, IntType (), Nil, stmts),
            Message ("type error: expected boolean got int"))
    }

    test ("the condition of a while statement can have Boolean type") {
        val exp = IntExp (0) // dummy
        val cond = TrueExp ()
        val stmts = Seq (While (cond, Block (Nil)))
        semanticTest (embedExpression (exp, IntType (), Nil, stmts))
    }

    test ("the condition of a while statement cannot have integer type") {
        val exp = IntExp (0) // dummy
        val cond = IntExp (42)
        val stmts = Seq (While (cond, Block (Nil)))
        semanticTest (
            embedExpression (exp, IntType (), Nil, stmts),
            Message ("type error: expected boolean got int"))
    }

    // Test type of expression in println statement can be of any type (Rule 8)

    test ("the expression in a println statement can be of Boolean type") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val stmts = Seq (Println (exp1))
        semanticTest (embedExpression (exp, IntType (), Nil, stmts))
    }

    test ("the expression in a println statement can be of integer type") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val stmts = Seq (Println (exp1))
        semanticTest (embedExpression (exp, IntType (), Nil, stmts))
    }

    test ("the expression in a println statement can be of integer array type") {
        val exp = IntExp (0) // dummy
        val exp1 = NewArrayExp (IntExp (42))
        val stmts = Seq (Println (exp1))
        semanticTest (embedExpression (exp, IntType (), Nil, stmts))
    }

    test ("the expression in a println statement can be of reference type") {
        val exp = IntExp (0) // dummy
        val exp1 = NewExp (IdnUse ("Test"))
        val stmts = Seq (Println (exp1))
        semanticTest (embedExpression (exp, IntType (), Nil, stmts))
    }

    // Test that we can't assign to a non-variable or argument (Rule 9)

    test ("a method name cannot be assigned to") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val vars = Seq ()
        val stmts = Seq (VarAssign (IdnUse ("m"), exp1))
        semanticTest (embedExpression (exp, IntType (), vars, stmts),
            Message ("illegal assignment to non-variable, non-argument"))
    }

    // Test that assignment RHSes have compatible types with LHS (Rule 9)

    test ("an integer expression is assignment compatible with an integer var") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val vars = Seq (Var (IntType (), IdnDef ("v")))
        val stmts = Seq (VarAssign (IdnUse ("v"), exp1))
        semanticTest (embedExpression (exp, IntType (), vars, stmts))
    }

    test ("a Boolean expression is not assignment compatible with an integer var") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val vars = Seq (Var (IntType (), IdnDef ("v")))
        val stmts = Seq (VarAssign (IdnUse ("v"), exp1))
        semanticTest (
            embedExpression (exp, IntType (), vars, stmts),
            Message ("type error: expected int got boolean"))
    }

    test ("a Boolean expression is assignment compatible with a Boolean var") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val vars = Seq (Var (BooleanType (), IdnDef ("v")))
        val stmts = Seq (VarAssign (IdnUse ("v"), exp1))
        semanticTest (embedExpression (exp, IntType (), vars, stmts))
    }

    test ("an integer expression is not assignment compatible with a Boolean var") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val vars = Seq (Var (BooleanType (), IdnDef ("v")))
        val stmts = Seq (VarAssign (IdnUse ("v"), exp1))
        semanticTest (
            embedExpression (exp, IntType (), vars, stmts),
            Message ("type error: expected boolean got int"))
    }

    test ("an integer array expression is assignment compatible with an integer array var") {
        val exp = IntExp (0) // dummy
        val exp1 = NewArrayExp (IntExp (42))
        val vars = Seq (Var (IntArrayType (), IdnDef ("v")))
        val stmts = Seq (VarAssign (IdnUse ("v"), exp1))
        semanticTest (embedExpression (exp, IntType (), vars, stmts))
    }

    test ("an integer expression is not assignment compatible with an integer array var") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val vars = Seq (Var (IntArrayType (), IdnDef ("v")))
        val stmts = Seq (VarAssign (IdnUse ("v"), exp1))
        semanticTest (
            embedExpression (exp, IntType (), vars, stmts),
            Message ("type error: expected int[] got int"))
    }

    // Test types in array assignments (Rule 10)

    test ("integer expressions are ok in an integer array assignment") {
        val exp = IntExp (0) // dummy
        val exp1 = IntExp (42)
        val exp2 = IntExp (99)
        val vars = Seq (Var (IntArrayType (), IdnDef ("v")))
        val stmts = Seq (ArrayAssign (IdnUse ("v"), exp1, exp2))
        semanticTest (embedExpression (exp, IntType (), vars, stmts))
    }

    test ("Boolean expressions are not ok in an integer array assignment") {
        val exp = IntExp (0) // dummy
        val exp1 = TrueExp ()
        val exp2 = FalseExp ()
        val vars = Seq (Var (IntArrayType (), IdnDef ("v")))
        val stmts = Seq (ArrayAssign (IdnUse ("v"), exp1, exp2))
        semanticTest (
            embedExpression (exp, IntType (), vars, stmts),
            Message ("type error: expected int got boolean"),
            Message ("type error: expected int got boolean"))
    }

    // Test type of plus expressions (Rule 11)

    test ("the children of a plus expression are allowed to be integers") {
        val exp = PlusExp (IntExp (42), IntExp (99))
        semanticTest (embedExpression (exp))
    }

    test ("the children of a plus expression must be integers and its type is integer") {
        val exp = PlusExp (TrueExp (), FalseExp ())
        val analysis =
            semanticTest (
                embedExpression (exp),
                Message ("type error: expected int got boolean"),
                Message ("type error: expected int got boolean"))
        assertResult (IntType ()) (analysis.tipe (exp))
    }

    // Test type of and expressions (Rule 12)

    test ("the children of an and expression are allowed to be Booleans") {
        val exp = AndExp (TrueExp (), FalseExp ())
        semanticTest (embedExpression (exp, BooleanType ()))
    }

    test ("the children of an and expression must be Booelans and its type is Boolean") {
        val exp = AndExp (IntExp (42), IntExp (99))
        val analysis =
            semanticTest (
                embedExpression (exp, BooleanType ()),
                Message ("type error: expected boolean got int"),
                Message ("type error: expected boolean got int"))
        assertResult (BooleanType ()) (analysis.tipe (exp))
    }

    // Test type of plus expressions (Rule 13)

    test ("the child of a not expression is allowed to be Boolean") {
        val exp = NotExp (TrueExp ())
        semanticTest (embedExpression (exp, BooleanType ()))
    }

    test ("the child of a not expression must be Boolean and its type is Boolean") {
        val exp = NotExp (IntExp (42))
        val analysis =
            semanticTest (
                embedExpression (exp, BooleanType ()),
                Message ("type error: expected boolean got int"))
        assertResult (BooleanType ()) (analysis.tipe (exp))
    }

    // Test type of less-than expressions (Rule 14)

    test ("the children of a less-than expression are allowed to be integers") {
        val exp = LessExp (IntExp (42), IntExp (99))
        semanticTest (embedExpression (exp, BooleanType ()))
    }

    test ("the children of a less-than expression must be integers and its type is Boolean") {
        val exp = LessExp (TrueExp (), FalseExp ())
        val analysis =
            semanticTest (
                embedExpression (exp, BooleanType ()),
                Message ("type error: expected int got boolean"),
                Message ("type error: expected int got boolean"))
        assertResult (BooleanType ()) (analysis.tipe (exp))
    }

    // Test type of length expressions (Rule 15)

    test ("the child of a length expression is allowed to be an integer array") {
        val exp = LengthExp (NewArrayExp (IntExp (42)))
        semanticTest (embedExpression (exp))
    }

    test ("the child of a length expression must be an integer array and its type is integer") {
        val exp = LengthExp (IntExp (42))
        val analysis =
            semanticTest (
                embedExpression (exp),
                Message ("type error: expected int[] got int"))
        assertResult (IntType ()) (analysis.tipe (exp))
    }

    // Test method call expressions (rule 3, 16)

    test ("a non-method cannot be called") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return this.v ();
            |    }
            |}
            """.stripMargin,
            Message ("illegal call to non-method", positionAt (6, 21)))
    }

    test ("a superclass method can be called") {
        semanticTest ("""
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
    }

    test ("the type of a method call expression is the method return type (1)") {
        semanticTest ("""
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
    }

    test ("the type of a method call expression is the method return type (2)") {
        semanticTest ("""
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
            """.stripMargin,
            Message ("type error: expected int got boolean", positionAt (9, 13)))
    }

    test ("the numbers of arguments in a call can match the declaration") {
        semanticTest ("""
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
    }

    test ("the numbers of arguments in a call must match the declaration") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m (int a, int b) {
            |        return 33;
            |    }
            |    public int n () {
            |        return this.m (42);
            |    }
            |}
            """.stripMargin,
            Message ("wrong number of arguments, got 1 but expected 2", positionAt (8, 21)))
    }

    test ("the types of arguments in a call must match the declaration") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m (boolean a, int[] b) {
            |        return 33;
            |    }
            |    public int n () {
            |        return this.m (42, 99);
            |    }
            |}
            """.stripMargin,
            Message ("type error: expected boolean got int", positionAt (8, 24)),
            Message ("type error: expected int[] got int", positionAt (8, 28)))
    }

    test ("forward references to methods work") {
        semanticTest ("""
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
    }

    // Test the type of "this" (rule 17)

    test ("the type of this is the current class") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return this;
            |    }
            |}
            """.stripMargin)
    }

    // Test the types in new integer array expressions (rule 18)

    test ("The type of a new array expression is an integer array") {
        val exp = NewArrayExp (IntExp (42))
        val analysis = semanticTest (embedExpression (exp, IntArrayType ()))
        assertResult (IntArrayType ()) (analysis.tipe (exp))
    }

    test ("The type of the parameter in a new integer array expression must be an integer") {
        val exp = NewArrayExp (TrueExp ())
        semanticTest (
            embedExpression (exp, IntArrayType ()),
            Message ("type error: expected int got boolean"))
    }

    // Test the use of names in new expressions (rule 19)

    test ("The name used in a new expression must refer to a class") {
        val exp = NewExp (IdnUse ("v"))
        val vars = Seq (Var (IntType (), IdnDef ("v")))
        semanticTest (
            embedExpression (exp, IntType (), vars),
            Message ("illegal instance creation of non-class type"))
    }

    test ("The type of a new expression is a reference to the created class") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return (new Test ());
            |    }
            |}
            """.stripMargin)
    }

    // Test the return type of a method (rule 20)

    test ("The return expression of a method can return the appropriate type") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return 42;
            |    }
            |}
            """.stripMargin)
    }

    test ("The return expression of a method cannot return an inappropriate type") {
        semanticTest ("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return true;
            |    }
            |}
            """.stripMargin,
            Message ("type error: expected int got boolean", positionAt (5, 16)))
    }

    // Pretty-printing of environments

    test ("The pretty-print of an environment at an expression contains the correct scopes") {
        val exp = IntExp (42)
        val vars = Seq (Var (IntType (), IdnDef ("v")))
        val prog = embedExpression (exp, IntType (), vars)
        val tree = new MiniJavaTree (prog)
        val analyser = new SemanticAnalyser (tree)
        assertResult ("""scope
            |    "v" -> VariableEntity(Var(int,IdnDef(v)))
            |scope
            |    "m" -> MethodEntity(Method(IdnDef(m),MethodBody(int,List(),List(Var(int,IdnDef(v))),List(),IntExp(42))))
            |scope
            |    "Dummy" -> MainClassEntity(MainClass(IdnDef(Dummy),Println(IntExp(0))))
            |    "Test" -> ClassEntity(Class(IdnDef(Test),None,ClassBody(List(),List(Method(IdnDef(m),MethodBody(int,List(),List(Var(int,IdnDef(v))),List(),IntExp(42)))))))""".stripMargin
        ) (pretty (analyser.env (exp)))
    }

    /**
     * Parse some test input as a program, run the semantic analyser
     * over the resulting tree (if the parse succeeds) and check that
     * the expected messages are produced. Returns the analysis object
     * so that more tests can be performed by caller.
     */
    def semanticTest (str : String, expected : Message*) : SemanticAnalyser =
        runSemanticChecks (assertParseReturn (str, parser), expected : _*)

    /**
     * Run the semantic analyser over a given tree and check that the
     * expected messages are produced. Returns the analysis object
     * so that more tests can be performed by caller.
     */
    def semanticTest (prog : Program, expected : Message*) : SemanticAnalyser =
        runSemanticChecks (prog, expected : _*)

    /**
     * Run the semantic checks on the given program.
     */
    def runSemanticChecks (prog : Program, expected : Message*) : SemanticAnalyser = {
        val tree = new MiniJavaTree (prog)
        val analyser = new SemanticAnalyser (tree)
        assertMessages (analyser.errors, expected : _*)
        analyser
    }

    /**
     * Construct a program by inserting the given expression into a return
     * statement of a method. The idea is that you construct the expression
     * outside and insert it into the program for checking. The optional
     * `retType`, `vars` and `stmts` arguments can be used to inject a return
     * type, variable declarations or statements into the method as well. The
     * return type defaults to integer and the variable and statement lists
     * to empty.
     */
    def embedExpression (exp : Expression,
                         retType : Type = IntType (),
                         vars : Seq[Var] = Nil,
                         stmts : Seq[Statement] = Nil) =
        Program (MainClass (IdnDef ("Dummy"), Println (IntExp (0))),
            Seq(
                Class (IdnDef ("Test"), None,
                    ClassBody (
                        Nil,
                        Seq (
                            Method (IdnDef ("m"),
                                MethodBody (
                                    retType,
                                    Nil,
                                    vars,
                                    stmts,
                                    exp)))))))

}
