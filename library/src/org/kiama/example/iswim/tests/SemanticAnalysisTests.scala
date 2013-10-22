/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2013 Dominic R B Verity, Anthony Sloane, Macquarie University.
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

import org.kiama.attribution.Attribution.initTree
import org.kiama.example.iswim.compiler._
import org.kiama.util.Tests

class SemanticAnalysisTests extends Tests with Parser {

    import org.kiama.util.{Message, Messaging}
    import Syntax.{Expr, Iswim}

    def runSemanticChecks (parseResult : ParseResult[Iswim], semanticallyCorrect : Boolean,
                           messages : (Int, Message)*) : SemanticAnalysis = {
        assert (parseResult.successful)
        val ast = parseResult.get
        initTree (ast)
        val messaging = new Messaging
        val analysis = new SemanticAnalysis (messaging)
        val correct = analysis.isSemanticallyCorrect (ast)
        assert (correct === semanticallyCorrect)
        assertMessages (messaging, messages : _*)
        analysis
    }

    test("simple test of use of a correctly bound variable") {
        val parseResult = parseAll(expr, "let x = 1 in x + x")
        runSemanticChecks (parseResult, true)
    }

    test("simple test of a recursive binding") {
        val parseResult = parseAll(expr, "letrec f = fun(x){ g x } and g = fun(y){ f y } in f 1")
        runSemanticChecks (parseResult, true)
    }

    test("test of top level bindings in which all variables correctly bound") {
        val parseResult = parseAll(parser,
""" let x = 1 and y = 60;

    let f = fun(x) { x + y };

    letrec g = fun(z) { if (z <= 0) 1 else x * g(z-1) };

    let main = fun(w) {
        f(20) + g(y)
    }
""")
        runSemanticChecks (parseResult, true)
    }

    test("simple test of use of an unbound variable in body of let") {
        val parseResult = parseAll(expr, "let x = 1 in x + y")
        runSemanticChecks (parseResult, false,
            (0, Message (1, 18, "unbound variable 'y'")))
    }

    test("use of an unbound variable in an expression being bound to a variable in a let") {
        val parseResult = parseAll(expr, "let x = y and z = x in z + x")
        runSemanticChecks (parseResult, false,
            (0, Message (1, 9, "unbound variable 'y'")),
            (1, Message (1, 19, "unbound variable 'x'")))
    }

    test("test of top level bindings in which some variables incorrectly bound") {
        val parseResult = parseAll(parser,
""" let x = 1 and y = 60;

    let f = fun(x) { x + w };

    let w = (let z = (let y = w in y + t) in (let m = t * z in (m + z1, z)));

    letrec g = fun(z) { if (z <= 0) 1 else x * g(z-1) * (k w) }
    and k = fun(z) { g(1) + m(2) };

    let main = fun(w) {
        f(20) + g(y)
    }
""")
        runSemanticChecks (parseResult, false,
            (0, Message (3, 26, "unbound variable 'w'")),
            (1, Message (5, 31, "unbound variable 'w'")),
            (2, Message (5, 40, "unbound variable 't'")),
            (3, Message (5, 55, "unbound variable 't'")),
            (4, Message (5, 69, "unbound variable 'z1'")),
            (5, Message (8, 29, "unbound variable 'm'")))
    }

    test("correct use of bound variables in a match expression") {
        val parseResult = parseAll(expr, """
    (1,2) match {
        ()      -> 34;
        (y,z)   -> y + z;
        x       -> x * x
    }
""")
        runSemanticChecks (parseResult, true)
    }

    test("unbound variables in a match expression") {
        val parseResult = parseAll(expr, """
    (1,2) match {
        ()      -> y;
        (y,z)   -> y + w;
        x       -> x * z
    }
""")
        runSemanticChecks (parseResult, false,
            (0, Message (3, 20, "unbound variable 'y'")),
            (1, Message (4, 24, "unbound variable 'w'")),
            (2, Message (5, 24, "unbound variable 'z'")))
    }

    test("unreachable clauses in a match expression") {
        val parseResult = parseAll(expr, """
    (1,2) match {
        ()      -> 1;
        w       -> w + 1;
        (y,z)   -> y + z;
        x       -> x / 10
    }
""")
        runSemanticChecks (parseResult, false,
            (0, Message (5, 9, "unreachable match clause")),
            (1, Message (6, 9, "unreachable match clause")))
    }

}

