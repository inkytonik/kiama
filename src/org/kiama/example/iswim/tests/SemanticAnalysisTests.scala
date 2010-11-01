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
 * Tests of semantic analysis attribution.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.kiama.example.iswim.compiler._

@RunWith(classOf[JUnitRunner])
class SemanticAnalysisTests extends FunSuite with SemanticAnalysis with Parser {

    import Syntax._
    import org.kiama.util.Messaging._

    /**
     * Assert that a message was produced at a given position.
     */
    def assertMessage (index : Int, line : Int, column : Int, msg : String) {
        val m = messages (index)
        expect (line, "wrong line number in message " + index) (m.pos.line)
        expect (column, "wrong column number in message " + index) (m.pos.column)
        expect (msg, "wrong text in message " + index) (m.message)
    }

    test("simple test of use of a correctly bound variable") {
        val prog = parseAll(expr, "let x = 1 in x + x")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === true)
        assert(messagecount === 0)
    }

    test("simple test of a recursive binding") {
        val prog = parseAll(expr, "letrec f = fun(x){ g x } and g = fun(y){ f y } in f 1")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === true)
        assert(messagecount === 0)
    }

    test("test of top level bindings in which all variables correctly bound") {
        val prog = parseAll(start,
""" let x = 1 and y = 60;

    let f = fun(x) { x + y };

    letrec g = fun(z) { if (z <= 0) 1 else x * g(z-1) };

    let main = fun(w) {
        f(20) + g(y)
    }
""")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === true)
        assert(messagecount === 0)
    }

    test("simple test of use of an unbound variable in body of let") {
        val prog = parseAll(expr, "let x = 1 in x + y")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === false)
        assert(messagecount === 1)
        assertMessage(0, 1, 18, "unbound variable 'y'")
    }

    test("use of an unbound variable in an expression being bound to a variable in a let") {
        val prog = parseAll(expr, "let x = y and z = x in z + x")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === false)
        assert(messagecount === 2)
        assertMessage(0, 1, 9, "unbound variable 'y'")
        assertMessage(1, 1, 19, "unbound variable 'x'")
    }

    test("test of top level bindings in which some variables incorrectly bound") {
        val prog = parseAll(start,
""" let x = 1 and y = 60;

    let f = fun(x) { x + w };

    let w = (let z = (let y = w in y + t) in (let m = t * z in (m + z1, z)));

    letrec g = fun(z) { if (z <= 0) 1 else x * g(z-1) * (k w) }
    and k = fun(z) { g(1) + m(2) };

    let main = fun(w) {
        f(20) + g(y)
    }
""")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === false)
        assert(messagecount === 6)
        assertMessage(0, 3, 26, "unbound variable 'w'")
        assertMessage(1, 5, 31, "unbound variable 'w'")
        assertMessage(2, 5, 40, "unbound variable 't'")
        assertMessage(3, 5, 55, "unbound variable 't'")
        assertMessage(4, 5, 69, "unbound variable 'z1'")
        assertMessage(5, 8, 29, "unbound variable 'm'")
    }

    test("correct use of bound variables in a match expression") {
        val prog = parseAll(expr, """
    (1,2) match {
        ()      -> 34;
        (y,z)   -> y + z;
        x       -> x * x
    }
""")
        assert(prog.successful)
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === true)
    }

    test("unbound variables in a match expression") {
        val prog = parseAll(expr, """
    (1,2) match {
        ()      -> y;
        (y,z)   -> y + w;
        x       -> x * z
    }
""")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === false)
        assert(messagecount === 3)
        assertMessage(0, 3, 20, "unbound variable 'y'")
        assertMessage(1, 4, 24, "unbound variable 'w'")
        assertMessage(2, 5, 24, "unbound variable 'z'")
    }

    test("unreachable clauses in a match expression") {
        val prog = parseAll(expr, """
    (1,2) match {
        ()      -> 1;
        w       -> w + 1;
        (y,z)   -> y + z;
        x       -> x / 10
    }
""")
        assert(prog.successful)
        resetmessages
        val result = (prog.get)->isSemanticallyCorrect
        assert(result === false)
        assert(messagecount === 2)
        assertMessage(0, 5, 9, "unreachable match clause")
        assertMessage(1, 6, 9, "unreachable match clause")
    }

}

