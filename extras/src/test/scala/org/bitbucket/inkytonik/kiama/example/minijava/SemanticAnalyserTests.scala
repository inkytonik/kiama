/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
    import org.bitbucket.inkytonik.kiama.util.{Messaging, Positions, StringSource}

    val positions = new Positions
    val messaging = new Messaging(positions)
    val parsers = new SyntaxAnalyser(positions)

    /**
     * Wrapper for data checked by MiniJava semantic tests.
     */
    case class MiniJavaMessage(label : String, line : Int = 0, column : Int = 0)

    /**
     * Parse and analyse the term string and return the messages that result.
     */
    def check(term : String) : String = {
        parsers.parseAll(parsers.program, StringSource(term)) match {
            case Success(program, _) =>
                val tree = new MiniJavaTree(program)
                val analyser = new SemanticAnalyser(tree)
                messaging.formatMessages(analyser.errors)
            case result : NoSuccess =>
                result.message
        }
    }

    /**
     * Analyse the expression after emebdding it in a program using
     * `embedExpression` and return the messages that result. The arguments
     * are as for `embedExpression`.
     */
    def errors(
        exp : Expression,
        retType : Type = IntType(),
        vars : Vector[Var] = Vector(),
        stmts : Vector[Statement] = Vector()
    ) : String = {
        val tree = new MiniJavaTree(embedExpression(exp, retType, vars, stmts))
        val analyser = new SemanticAnalyser(tree)
        messaging.formatMessages(analyser.errors)
    }

    /**
     * Query the type of an expression. The expression is embedded in a
     * program using `embedExpression`.  The arguments are as for
     * `embedExpression`.
     */
    def typeOf(
        exp : Expression,
        retType : Type = IntType(),
        vars : Vector[Var] = Vector(),
        stmts : Vector[Statement] = Vector()
    ) : Type = {
        val tree = new MiniJavaTree(embedExpression(exp))
        val analyser = new SemanticAnalyser(tree)
        analyser.tipe(exp)
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

    // Tests of definition uniqueness (Rule 1)

    test("two declarations of same class is an error") {
        check("""
            |class Main { public static void main () { System.out.println (0); } }
            |class Main { }
            """.stripMargin) shouldBe (
            """2:7:error: Main is declared more than once
              |class Main { public static void main () { System.out.println (0); } }
              |      ^
              |3:7:error: Main is declared more than once
              |class Main { }
              |      ^
              |""".stripMargin
        )
    }

    test("two declarations of same name in same class is an error") {
        check(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int mult;
            |    int mult;
            |}
            """.stripMargin
        ) shouldBe (
                """4:9:error: mult is declared more than once
              |    int mult;
              |        ^
              |5:9:error: mult is declared more than once
              |    int mult;
              |        ^
              |""".stripMargin
            )
    }

    test("two declarations of same name in different scopes is ok") {
        check("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int notmult;
            |    public int m () {
            |        int notmult;
            |        return 0;
            |    }
            |}
            """.stripMargin) shouldBe ""
    }

    // Test of applied occurence matching defining occurrence (Rule 2)

    test("use of a name that is not declared is an error") {
        check(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        notdecl = 1;
            |        return 0;
            |    }
            |}
            """.stripMargin
        ) shouldBe (
                """5:9:error: notdecl is not declared
              |        notdecl = 1;
              |        ^
              |""".stripMargin
            )
    }

    test("use of a name that is declared in wrong scope is an error") {
        check(
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
            """.stripMargin
        ) shouldBe (
                """9:9:error: notdecl is not declared
              |        notdecl = 1;
              |        ^
              |""".stripMargin
            )
    }

    // Test type of integer expression (Rule 4)

    test("an integer expression has integer type") {
        typeOf(IntExp(42)) shouldBe IntType()
    }

    // Test type of boolean expressions (Rule 5)

    test("a true expression has Boolean type") {
        typeOf(TrueExp()) shouldBe BooleanType()
    }

    test("a false expression has Boolean type") {
        typeOf(FalseExp()) shouldBe BooleanType()
    }

    // Test use of method names in expressions (rule 6)

    test("a method name cannot be used in an expression") {
        check(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return m;
            |    }
            |}
            """.stripMargin
        ) shouldBe (
                """6:16:error: can't refer to methods directly
              |        return m;
              |               ^
              |""".stripMargin
            )
    }

    // Test type of condition in if and while statements (Rule 7)

    test("the condition of an if statement can have Boolean type") {
        val exp = IntExp(0) // dummy
        val cond = TrueExp()
        val stmts = Vector(If(cond, Block(Vector()), Block(Vector())))
        errors(exp, IntType(), Vector(), stmts) shouldBe ("")
    }

    test("the condition of an if statement cannot have integer type") {
        val exp = IntExp(0) // dummy
        val cond = IntExp(42)
        val stmts = Vector(If(cond, Block(Vector()), Block(Vector())))
        errors(exp, IntType(), Vector(), stmts) shouldBe (
            "expected boolean type got int\n"
        )
    }

    test("the condition of a while statement can have Boolean type") {
        val exp = IntExp(0) // dummy
        val cond = TrueExp()
        val stmts = Vector(While(cond, Block(Vector())))
        errors(exp, IntType(), Vector(), stmts) shouldBe ("")
    }

    test("the condition of a while statement cannot have integer type") {
        val exp = IntExp(0) // dummy
        val cond = IntExp(42)
        val stmts = Vector(While(cond, Block(Vector())))
        errors(exp, IntType(), Vector(), stmts) shouldBe (
            "expected boolean type got int\n"
        )
    }

    // Test type of expression in println statement can be of any type (Rule 8)

    test("the expression in a println statement can be of Boolean type") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val stmts = Vector(Println(exp1))
        errors(exp, IntType(), Vector(), stmts) shouldBe ("")
    }

    test("the expression in a println statement can be of integer type") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val stmts = Vector(Println(exp1))
        errors(exp, IntType(), Vector(), stmts) shouldBe ("")
    }

    test("the expression in a println statement can be of integer array type") {
        val exp = IntExp(0) // dummy
        val exp1 = NewArrayExp(IntExp(42))
        val stmts = Vector(Println(exp1))
        errors(exp, IntType(), Vector(), stmts) shouldBe ("")
    }

    test("the expression in a println statement can be of reference type") {
        val exp = IntExp(0) // dummy
        val exp1 = NewExp(IdnUse("Test"))
        val stmts = Vector(Println(exp1))
        errors(exp, IntType(), Vector(), stmts) shouldBe ("")
    }

    // Test that we can't assign to a non-variable or argument (Rule 9)

    test("a method name cannot be assigned to") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector()
        val stmts = Vector(VarAssign(IdnUse("m"), exp1))
        errors(exp, IntType(), vars, stmts) shouldBe (
            "illegal assignment to non-variable, non-argument\n"
        )
    }

    // Test that assignment RHSes have compatible types with LHS (Rule 9)

    test("an integer expression is assignment compatible with an integer var") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector(Var(IntType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        errors(exp, IntType(), vars, stmts) shouldBe ("")
    }

    test("a Boolean expression is not assignment compatible with an integer var") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val vars = Vector(Var(IntType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        errors(exp, IntType(), vars, stmts) shouldBe (
            "expected int type got boolean\n"
        )
    }

    test("a Boolean expression is assignment compatible with a Boolean var") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val vars = Vector(Var(BooleanType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        errors(exp, IntType(), vars, stmts) shouldBe ("")
    }

    test("an integer expression is not assignment compatible with a Boolean var") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector(Var(BooleanType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        errors(exp, IntType(), vars, stmts) shouldBe (
            "expected boolean type got int\n"
        )
    }

    test("an integer array expression is assignment compatible with an integer array var") {
        val exp = IntExp(0) // dummy
        val exp1 = NewArrayExp(IntExp(42))
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        errors(exp, IntType(), vars, stmts) shouldBe ("")
    }

    test("an integer expression is not assignment compatible with an integer array var") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(VarAssign(IdnUse("v"), exp1))
        errors(exp, IntType(), vars, stmts) shouldBe (
            "expected int[] type got int\n"
        )
    }

    // Test types in array assignments (Rule 10)

    test("integer expressions are ok in an integer array assignment") {
        val exp = IntExp(0) // dummy
        val exp1 = IntExp(42)
        val exp2 = IntExp(99)
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(ArrayAssign(IdnExp(IdnUse("v")), exp1, exp2))
        errors(exp, IntType(), vars, stmts) shouldBe ("")
    }

    test("Boolean expressions are not ok in an integer array assignment") {
        val exp = IntExp(0) // dummy
        val exp1 = TrueExp()
        val exp2 = FalseExp()
        val vars = Vector(Var(IntArrayType(), IdnDef("v")))
        val stmts = Vector(ArrayAssign(IdnExp(IdnUse("v")), exp1, exp2))
        errors(exp, IntType(), vars, stmts) shouldBe (
            "expected int type got boolean\nexpected int type got boolean\n"
        )
    }

    // Test type of plus expressions (Rule 11)

    test("the children of a plus expression are allowed to be integers") {
        val exp = PlusExp(IntExp(42), IntExp(99))
        errors(exp) shouldBe ("")
    }

    test("the children of a plus expression must be integers and its type is integer") {
        val exp = PlusExp(TrueExp(), FalseExp())
        errors(exp) shouldBe (
            "expected int type got boolean\nexpected int type got boolean\n"
        )
        typeOf(exp) shouldBe IntType()
    }

    // Test type of and expressions (Rule 12)

    test("the children of an and expression are allowed to be Booleans") {
        val exp = AndExp(TrueExp(), FalseExp())
        errors(exp, BooleanType()) shouldBe ("")
    }

    test("the children of an and expression must be Booelans and its type is Boolean") {
        val exp = AndExp(IntExp(42), IntExp(99))
        errors(exp, BooleanType()) shouldBe (
            "expected boolean type got int\nexpected boolean type got int\n"
        )
        typeOf(exp) shouldBe BooleanType()
    }

    // Test type of plus expressions (Rule 13)

    test("the child of a not expression is allowed to be Boolean") {
        val exp = NotExp(TrueExp())
        errors(exp, BooleanType()) shouldBe ("")
    }

    test("the child of a not expression must be Boolean and its type is Boolean") {
        val exp = NotExp(IntExp(42))
        errors(exp, BooleanType()) shouldBe (
            "expected boolean type got int\n"
        )
        typeOf(exp) shouldBe BooleanType()
    }

    // Test type of less-than expressions (Rule 14)

    test("the children of a less-than expression are allowed to be integers") {
        val exp = LessExp(IntExp(42), IntExp(99))
        errors(exp, BooleanType()) shouldBe ("")
    }

    test("the children of a less-than expression must be integers and its type is Boolean") {
        val exp = LessExp(TrueExp(), FalseExp())
        errors(exp, BooleanType()) shouldBe (
            "expected int type got boolean\nexpected int type got boolean\n"
        )
        typeOf(exp) shouldBe BooleanType()
    }

    // Test type of length expressions (Rule 15)

    test("the child of a length expression is allowed to be an integer array") {
        val exp = LengthExp(NewArrayExp(IntExp(42)))
        errors(exp) shouldBe ("")
    }

    test("the child of a length expression must be an integer array and its type is integer") {
        val exp = LengthExp(IntExp(42))
        errors(exp) shouldBe (
            "expected int[] type got int\n"
        )
        typeOf(exp) shouldBe IntType()
    }

    // Test method call expressions (rule 3, 16)

    test("a non-method cannot be called") {
        check(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    int v;
            |    public int m () {
            |        return this.v ();
            |    }
            |}
            """.stripMargin
        ) shouldBe (
                """6:21:error: illegal call to non-method
              |        return this.v ();
              |                    ^
              |""".stripMargin
            )
    }

    test("a superclass method can be called") {
        check("""
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
            """.stripMargin) shouldBe ("")
    }

    test("the type of a method call expression is the method return type (1)") {
        check("""
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
            """.stripMargin) shouldBe ("")
    }

    test("the type of a method call expression is the method return type (2)") {
        check(
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
            """.stripMargin
        ) shouldBe (
                """9:13:error: expected int type got boolean
             |        v = this.m ();
             |            ^
             |""".stripMargin
            )
    }

    test("the numbers of arguments in a call can match the declaration") {
        check("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m (int a, int b) {
            |        return 33;
            |    }
            |    public int n () {
            |        return this.m (42, 99);
            |    }
            |}
            """.stripMargin) shouldBe ("")
    }

    test("the numbers of arguments in a call must match the declaration") {
        check(
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
            """.stripMargin
        ) shouldBe (
                """8:21:error: wrong number of arguments, got 1 but expected 2
              |        return this.m (42);
              |                    ^
              |""".stripMargin
            )
    }

    test("the types of arguments in a call must match the declaration") {
        check(
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
            """.stripMargin
        ) shouldBe (
                """8:24:error: expected boolean type got int
              |        return this.m (42, 99);
              |                       ^
              |8:28:error: expected int[] type got int
              |        return this.m (42, 99);
              |                           ^
              |""".stripMargin
            )
    }

    test("forward references to methods work") {
        check("""
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
            """.stripMargin) shouldBe ("")
    }

    // Test the type of "this" (rule 17)

    test("the type of this is the current class") {
        check("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return this;
            |    }
            |}
            """.stripMargin) shouldBe ("")
    }

    // Test the types in new integer array expressions (rule 18)

    test("The type of a new array expression is an integer array") {
        val exp = NewArrayExp(IntExp(42))
        typeOf(exp, IntArrayType()) shouldBe IntArrayType()
    }

    test("The type of the parameter in a new integer array expression must be an integer") {
        val exp = NewArrayExp(TrueExp())
        errors(exp, IntArrayType()) shouldBe (
            "expected int type got boolean\n"
        )
    }

    // Test the use of names in new expressions (rule 19)

    test("The name used in a new expression must refer to a class") {
        val exp = NewExp(IdnUse("v"))
        val vars = Vector(Var(IntType(), IdnDef("v")))
        errors(exp, IntType(), vars) shouldBe (
            "illegal instance creation of non-class type\n"
        )
    }

    test("The type of a new expression is a reference to the created class") {
        check("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public Test m () {
            |        return (new Test ());
            |    }
            |}
            """.stripMargin) shouldBe ("")
    }

    // Test the return type of a method (rule 20)

    test("The return expression of a method can return the appropriate type") {
        check("""
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return 42;
            |    }
            |}
            """.stripMargin) shouldBe ("")
    }

    test("The return expression of a method cannot return an inappropriate type") {
        check(
            """
            |class Dummy { public static void main () { System.out.println (0); } }
            |class Test {
            |    public int m () {
            |        return true;
            |    }
            |}
            """.stripMargin
        ) shouldBe (
                """5:16:error: expected int type got boolean
              |        return true;
              |               ^
              |""".stripMargin
            )
    }

    test("A lowercase-starting class name produces an appropriate information message") {
        check(
            """
            |class dummy { public static void main () { System.out.println (0); } }
            """.stripMargin
        ) shouldBe (
                """2:7:info: Style guides suggest starting class names with an upper case letter
              |class dummy { public static void main () { System.out.println (0); } }
              |      ^
              |""".stripMargin
            )
    }

    // Pretty-printing of environments

    test("The pretty-print of an environment at an expression contains the correct scopes") {
        val exp = IntExp(42)
        val vars = Vector(Var(IntType(), IdnDef("v")))
        val prog = embedExpression(exp, IntType(), vars)
        val tree = new MiniJavaTree(prog)
        val analyser = new SemanticAnalyser(tree)
        format(analyser.env(exp)) shouldBe
            """scope
            |    "v" -> VariableEntity(Var(int,IdnDef(v)))
            |scope
            |    "m" -> MethodEntity(Method(IdnDef(m),MethodBody(int,Vector(),Vector(Var(int,IdnDef(v))),Vector(),Result(IntExp(42)))))
            |scope
            |    "Dummy" -> MainClassEntity(MainClass(IdnDef(Dummy),MainMethod(Println(IntExp(0)))))
            |    "Test" -> ClassEntity(Class(IdnDef(Test),None,ClassBody(Vector(),Vector(Method(IdnDef(m),MethodBody(int,Vector(),Vector(Var(int,IdnDef(v))),Vector(),Result(IntExp(42))))))))""".stripMargin
    }

}
