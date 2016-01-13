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

package org.bitbucket.inkytonik.kiama
package example.minijava

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
class SemanticAnalyserTests extends ParseTests {

    import MiniJavaTree._
    import SymbolTable.format
    import org.bitbucket.inkytonik.kiama.util.Message
    import org.bitbucket.inkytonik.kiama.util.{Position, StringSource}

    val parsers = new SyntaxAnalyser(positions)

    /**
     * Wrapper for data checked by MiniJava semantic tests.
     */
    case class MiniJavaMessage(label : String, line : Int = 0, column : Int = 0)

    // Tests of definition uniqueness (Rule 1)

    test("two declarations of same class is an error") {
        semanticTest(
            """
            |class Main { public static void main () { System.out.println (0); } }
            |class Main { }
            """.stripMargin,
            MiniJavaMessage("Main is declared more than once", 2, 7),
            MiniJavaMessage("Main is declared more than once", 3, 7)
        )
    }

    test("two declarations of same name in same class is an error") {
        semanticTest(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int mult;
            |    int mult;
            |}
            """.stripMargin,
            MiniJavaMessage("mult is declared more than once", 4, 9),
            MiniJavaMessage("mult is declared more than once", 5, 9)
        )
    }

    test("two declarations of same name in different scopes is ok") {
        semanticTest("""
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

    test("use of a name that is not declared is an error") {
        semanticTest(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        notdecl = 1;
            |        return 0;
            |    }
            |}
            """.stripMargin,
            MiniJavaMessage("notdecl is not declared", 5, 9)
        )
    }

    test("use of a name that is declared in wrong scope is an error") {
        semanticTest(
            """
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
            MiniJavaMessage("notdecl is not declared", 9, 9)
        )
    }

    // Test type of integer expression (Rule 4)

    test("an integer expression has integer type") {
        val exp = IntExp(42)
        val analysis = semanticTest(embedExpression(exp))
        assertResult(IntType())(analysis.tipe(exp))
    }

    // Test type of boolean expressions (Rule 5)

    test("a true expression has Boolean type") {
        val exp = TrueExp()
        val analysis = semanticTest(embedExpression(exp, BooleanType()))
        assertResult(BooleanType())(analysis.tipe(exp))
    }

    test("a false expression has Boolean type") {
        val exp = FalseExp()
        val analysis = semanticTest(embedExpression(exp, BooleanType()))
        assertResult(BooleanType())(analysis.tipe(exp))
    }

    // Test use of method names in expressions (rule 6)

    test("a method name cannot be used in an expression") {
        semanticTest(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return m;
            |    }
            |}
            """.stripMargin,
            MiniJavaMessage("can't refer to methods directly", 6, 16)
        )
    }

    // Test type of condition in if and while statements (Rule 7)

    test("the condition of an if statement can have Boolean type") {
        val exp = IntExp(0) // dummy
        val cond = TrueExp()
        val stmts = Vector(If(cond, Block(Vector()), Block(Vector())))
        semanticTest(embedExpression(exp, IntType(), Vector(), stmts))
    }

    test("the condition of an if statement cannot have integer type") {
        val exp = IntExp(0) // dummy
        val cond = IntExp(42)
        val stmts = Vector(If(cond, Block(Vector()), Block(Vector())))
        semanticTest(
            embedExpression(exp, IntType(), Vector(), stmts),
            MiniJavaMessage("type error: expected boolean got int")
        )
    }

    test("the condition of a while statement can have Boolean type") {
        val exp = IntExp(0) // dummy
        val cond = TrueExp()
        val stmts = Vector(While(cond, Block(Vector())))
        semanticTest(embedExpression(exp, IntType(), Vector(), stmts))
    }

    test("the condition of a while statement cannot have integer type") {
        val exp = IntExp(0) // dummy
        val cond = IntExp(42)
        val stmts = Vector(While(cond, Block(Vector())))
        semanticTest(
            embedExpression(exp, IntType(), Vector(), stmts),
            MiniJavaMessage("type error: expected boolean got int")
        )
    }

    // Test type of expression in println statement can be of any type (Rule 8)

    test("the expression in a println statement can be of Boolean type") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val stmts = Vector(Println(exp1))
        semanticTest(embedExpression(exp, IntType(), Vector(), stmts))
    }

    test("the expression in a println statement can be of integer type") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val stmts = Vector(Println(exp1))
        semanticTest(embedExpression(exp, IntType(), Vector(), stmts))
    }

    test("the expression in a println statement can be of integer array type") {
        val exp = IntExp(0) // dummy
        val exp1 = NewArrayExp(IntExp(42))
        val stmts = Vector(Println(exp1))
        semanticTest(embedExpression(exp, IntType(), Vector(), stmts))
    }

    test("the expression in a println statement can be of reference type") {
        val exp = IntExp(0) // dummy
        val exp1 = NewExp(IdnUse("Test"))
        val stmts = Vector(Println(exp1))
        semanticTest(embedExpression(exp, IntType(), Vector(), stmts))
    }

    // Test that we can't assign to a non-variable or argument (Rule 9)

    test("a method name cannot be assigned to") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector()
        val stmts = Vector(VarAssign(IdnUse("m"), exp1))
        semanticTest(
            embedExpression(exp, IntType(), vars, stmts),
            MiniJavaMessage("illegal assignment to non-variable, non-argument")
        )
    }

    // Test that assignment RHSes have compatible types with LHS (Rule 9)

    test("an integer expression is assignment compatible with an integer var") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector(Var(IntType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        semanticTest(embedExpression(exp, IntType(), vars, stmts))
    }

    test("a Boolean expression is not assignment compatible with an integer var") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val vars = Vector(Var(IntType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        semanticTest(
            embedExpression(exp, IntType(), vars, stmts),
            MiniJavaMessage("type error: expected int got boolean")
        )
    }

    test("a Boolean expression is assignment compatible with a Boolean var") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val vars = Vector(Var(BooleanType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        semanticTest(embedExpression(exp, IntType(), vars, stmts))
    }

    test("an integer expression is not assignment compatible with a Boolean var") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector(Var(BooleanType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        semanticTest(
            embedExpression(exp, IntType(), vars, stmts),
            MiniJavaMessage("type error: expected boolean got int")
        )
    }

    test("an integer array expression is assignment compatible with an integer array var") {
        val exp = IntExp(0) // dummy
        val exp1 = NewArrayExp(IntExp(42))
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        semanticTest(embedExpression(exp, IntType(), vars, stmts))
    }

    test("an integer expression is not assignment compatible with an integer array var") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        semanticTest(
            embedExpression(exp, IntType(), vars, stmts),
            MiniJavaMessage("type error: expected int[] got int")
        )
    }

    // Test types in array assignments (Rule 10)

    test("integer expressions are ok in an integer array assignment") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val exp2 = IntExp(99)
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(ArrayAssign(IdnUse("v"), exp1, exp2))
        semanticTest(embedExpression(exp, IntType(), vars, stmts))
    }

    test("Boolean expressions are not ok in an integer array assignment") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val exp2 = FalseExp()
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(ArrayAssign(IdnUse("v"), exp1, exp2))
        semanticTest(
            embedExpression(exp, IntType(), vars, stmts),
            MiniJavaMessage("type error: expected int got boolean"),
            MiniJavaMessage("type error: expected int got boolean")
        )
    }

    // Test type of plus expressions (Rule 11)

    test("the children of a plus expression are allowed to be integers") {
        val exp = PlusExp(IntExp(42), IntExp(99))
        semanticTest(embedExpression(exp))
    }

    test("the children of a plus expression must be integers and its type is integer") {
        val exp = PlusExp(TrueExp(), FalseExp())
        val analysis =
            semanticTest(
                embedExpression(exp),
                MiniJavaMessage("type error: expected int got boolean"),
                MiniJavaMessage("type error: expected int got boolean")
            )
        assertResult(IntType())(analysis.tipe(exp))
    }

    // Test type of and expressions (Rule 12)

    test("the children of an and expression are allowed to be Booleans") {
        val exp = AndExp(TrueExp(), FalseExp())
        semanticTest(embedExpression(exp, BooleanType()))
    }

    test("the children of an and expression must be Booelans and its type is Boolean") {
        val exp = AndExp(IntExp(42), IntExp(99))
        val analysis =
            semanticTest(
                embedExpression(exp, BooleanType()),
                MiniJavaMessage("type error: expected boolean got int"),
                MiniJavaMessage("type error: expected boolean got int")
            )
        assertResult(BooleanType())(analysis.tipe(exp))
    }

    // Test type of plus expressions (Rule 13)

    test("the child of a not expression is allowed to be Boolean") {
        val exp = NotExp(TrueExp())
        semanticTest(embedExpression(exp, BooleanType()))
    }

    test("the child of a not expression must be Boolean and its type is Boolean") {
        val exp = NotExp(IntExp(42))
        val analysis =
            semanticTest(
                embedExpression(exp, BooleanType()),
                MiniJavaMessage("type error: expected boolean got int")
            )
        assertResult(BooleanType())(analysis.tipe(exp))
    }

    // Test type of less-than expressions (Rule 14)

    test("the children of a less-than expression are allowed to be integers") {
        val exp = LessExp(IntExp(42), IntExp(99))
        semanticTest(embedExpression(exp, BooleanType()))
    }

    test("the children of a less-than expression must be integers and its type is Boolean") {
        val exp = LessExp(TrueExp(), FalseExp())
        val analysis =
            semanticTest(
                embedExpression(exp, BooleanType()),
                MiniJavaMessage("type error: expected int got boolean"),
                MiniJavaMessage("type error: expected int got boolean")
            )
        assertResult(BooleanType())(analysis.tipe(exp))
    }

    // Test type of length expressions (Rule 15)

    test("the child of a length expression is allowed to be an integer array") {
        val exp = LengthExp(NewArrayExp(IntExp(42)))
        semanticTest(embedExpression(exp))
    }

    test("the child of a length expression must be an integer array and its type is integer") {
        val exp = LengthExp(IntExp(42))
        val analysis =
            semanticTest(
                embedExpression(exp),
                MiniJavaMessage("type error: expected int[] got int")
            )
        assertResult(IntType())(analysis.tipe(exp))
    }

    // Test method call expressions (rule 3, 16)

    test("a non-method cannot be called") {
        semanticTest(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return this.v ();
            |    }
            |}
            """.stripMargin,
            MiniJavaMessage("illegal call to non-method", 6, 21)
        )
    }

    test("a superclass method can be called") {
        semanticTest("""
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

    test("the type of a method call expression is the method return type (1)") {
        semanticTest("""
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

    test("the type of a method call expression is the method return type (2)") {
        semanticTest(
            """
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
            MiniJavaMessage("type error: expected int got boolean", 9, 13)
        )
    }

    test("the numbers of arguments in a call can match the declaration") {
        semanticTest("""
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

    test("the numbers of arguments in a call must match the declaration") {
        semanticTest(
            """
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
            MiniJavaMessage("wrong number of arguments, got 1 but expected 2", 8, 21)
        )
    }

    test("the types of arguments in a call must match the declaration") {
        semanticTest(
            """
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
            MiniJavaMessage("type error: expected boolean got int", 8, 24),
            MiniJavaMessage("type error: expected int[] got int", 8, 28)
        )
    }

    test("forward references to methods work") {
        semanticTest("""
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

    test("the type of this is the current class") {
        semanticTest("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return this;
            |    }
            |}
            """.stripMargin)
    }

    // Test the types in new integer array expressions (rule 18)

    test("The type of a new array expression is an integer array") {
        val exp = NewArrayExp(IntExp(42))
        val analysis = semanticTest(embedExpression(exp, IntArrayType()))
        assertResult(IntArrayType())(analysis.tipe(exp))
    }

    test("The type of the parameter in a new integer array expression must be an integer") {
        val exp = NewArrayExp(TrueExp())
        semanticTest(
            embedExpression(exp, IntArrayType()),
            MiniJavaMessage("type error: expected int got boolean")
        )
    }

    // Test the use of names in new expressions (rule 19)

    test("The name used in a new expression must refer to a class") {
        val exp = NewExp(IdnUse("v"))
        val vars = Vector(Var(IntType(), IdnDef("v")))
        semanticTest(
            embedExpression(exp, IntType(), vars),
            MiniJavaMessage("illegal instance creation of non-class type")
        )
    }

    test("The type of a new expression is a reference to the created class") {
        semanticTest("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return (new Test ());
            |    }
            |}
            """.stripMargin)
    }

    // Test the return type of a method (rule 20)

    test("The return expression of a method can return the appropriate type") {
        semanticTest("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return 42;
            |    }
            |}
            """.stripMargin)
    }

    test("The return expression of a method cannot return an inappropriate type") {
        semanticTest(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return true;
            |    }
            |}
            """.stripMargin,
            MiniJavaMessage("type error: expected int got boolean", 5, 16)
        )
    }

    // Pretty-printing of environments

    test("The pretty-print of an environment at an expression contains the correct scopes") {
        val exp = IntExp(42)
        val vars = Vector(Var(IntType(), IdnDef("v")))
        val prog = embedExpression(exp, IntType(), vars)
        val tree = new MiniJavaTree(prog)
        val analyser = new SemanticAnalyser(tree)
        assertResult("""scope
            |    "v" -> VariableEntity(Var(int,IdnDef(v)))
            |scope
            |    "m" -> MethodEntity(Method(IdnDef(m),MethodBody(int,Vector(),Vector(Var(int,IdnDef(v))),Vector(),Result(IntExp(42)))))
            |scope
            |    "Dummy" -> MainClassEntity(MainClass(IdnDef(Dummy),MainMethod(Println(IntExp(0)))))
            |    "Test" -> ClassEntity(Class(IdnDef(Test),None,ClassBody(Vector(),Vector(Method(IdnDef(m),MethodBody(int,Vector(),Vector(Var(int,IdnDef(v))),Vector(),Result(IntExp(42))))))))""".stripMargin)(format(analyser.env(exp)))
    }

    /**
     * Parse some test input as a program, run the semantic analyser
     * over the resulting tree (if the parse succeeds) and check that
     * the expected messages are produced. Returns the analysis object
     * so that more tests can be performed by caller.
     */
    def semanticTest(str : String, expected : MiniJavaMessage*) : SemanticAnalyser =
        runSemanticChecks(assertParseReturn(str, parsers.program), expected : _*)

    /**
     * Run the semantic analyser over a given tree and check that the
     * expected messages are produced. Returns the analysis object
     * so that more tests can be performed by caller.
     */
    def semanticTest(prog : Program, expected : MiniJavaMessage*) : SemanticAnalyser =
        runSemanticChecks(prog, expected : _*)

    /**
     * Run the semantic checks on the given program.
     */
    def runSemanticChecks(prog : Program, expected : MiniJavaMessage*) : SemanticAnalyser = {
        val tree = new MiniJavaTree(prog)
        val analyser = new SemanticAnalyser(tree)
        val source = new StringSource("dummy")
        val expectedMessages = expected.map {
            case m @ MiniJavaMessage(label, l, c) =>
                if (l != 0) {
                    val pos = Position(l, c, source)
                    positions.setStart(m, pos)
                }
                Message(m, label)
        }
        assertMessages(analyser.errors, expectedMessages : _*)
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
    def embedExpression(
        exp : Expression,
        retType : Type = IntType(),
        vars : Vector[Var] = Vector(),
        stmts : Vector[Statement] = Vector()
    ) =
        Program(
            MainClass(IdnDef("Dummy"), MainMethod(Println(IntExp(0)))),
            Vector(
                Class(IdnDef("Test"), None,
                    ClassBody(
                        Vector(),
                        Vector(
                            Method(
                                IdnDef("m"),
                                MethodBody(
                                    retType,
                                    Vector(),
                                    vars,
                                    stmts,
                                    Result(exp)
                                )
                            )
                        )
                    ))
            )
        )

}
