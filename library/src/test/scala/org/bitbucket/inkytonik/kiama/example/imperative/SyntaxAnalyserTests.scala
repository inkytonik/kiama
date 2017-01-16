/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2017 Anthony M Sloane, Macquarie University.
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
package example.imperative

import ImperativeTree._
import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Imperative language basic syntax analysis tests.
 */
class SyntaxAnalyserTests extends PrettyPrinter with ParseTests {

    val parsers = new SyntaxAnalyser(positions)
    import parsers._

    def parseToStmt(s : Stmt) = parseTo(s)
    def parseToExp(e : Exp) = parseTo(e)

    test("an empty statement") {
        stmt(";") should parseToStmt(Null())
    }

    test("an assignment statement parses correctly") {
        asgnStmt("a = 1;") should parseToStmt(Asgn(Var("a"), Num(1)))
    }

    test("a while statement parses correctly") {
        whileStmt("while (a) a = 1;") should parseToStmt(While(Var("a"), Asgn(Var("a"), Num(1))))
    }

    test("an empty statement sequence parses correcrtly") {
        sequence("{}") should parseToStmt(Seqn(Vector()))
    }

    test("a singleton statement sequence parses correcrtly") {
        sequence("{ x = 1; }") should parseToStmt(Seqn(Vector(Asgn(Var("x"), Num(1)))))
    }

    test("a non-empty statement sequence parses correcrtly") {
        sequence("{ x = 1; y = 2; }") should parseToStmt(Seqn(Vector(Asgn(Var("x"), Num(1)), Asgn(Var("y"), Num(2)))))
    }

    test("a combination expression parses correctly (assoc)") {
        exp("1 + 2 + 3") should parseToExp(Add(Add(Num(1), Num(2)), Num(3)))
    }

    test("a combination expression parses correctly (prec left)") {
        exp("1 * 2 + 3") should parseToExp(Add(Mul(Num(1), Num(2)), Num(3)))
    }

    test("a combination expression parses correctly (prec right)") {
        exp("1 + 2 * 3") should parseToExp(Add(Num(1), Mul(Num(2), Num(3))))
    }

    test("a combination expression parses correctly (paren override prec left)") {
        exp("1 * (2 + 3)") should parseToExp(Mul(Num(1), Add(Num(2), Num(3))))
    }

    test("a combination expression parses correctly (paren override right)") {
        exp("(1 + 2) * 3") should parseToExp(Mul(Add(Num(1), Num(2)), Num(3)))
    }

    test("an addition expression parses correctly") {
        exp("8.9 + 0") should parseToExp(Add(Num(8.9), Num(0)))
    }

    test("a subtraction expression parses correctly") {
        exp("99 - 3.45") should parseToExp(Sub(Num(99), Num(3.45)))
    }

    test("a multiplication expression parses correctly") {
        exp("a * 1.2") should parseToExp(Mul(Var("a"), Num(1.2)))
    }

    test("a division expression parses correctly") {
        exp("5 / x") should parseToExp(Div(Num(5), Var("x")))
    }

    test("a negation expression parses correctly") {
        exp("-10") should parseToExp(Neg(Num(10)))
    }

    test("a double parses correctly") {
        double("12.345") should parseTo(Num(12.345))
    }

    test("an integer parses correctly") {
        integer("12345") should parseTo(Num(12345))
    }

    test("a variable parses correctly") {
        variable("total") should parseTo(Var("total"))
    }

    test("a keyword doesn't parse as an identifier") {
        idn("while") should failParseAt(1, 1, "failure of not")
    }

}
