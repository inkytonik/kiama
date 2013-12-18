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
import org.kiama.util.RegexParserTests

class SemanticAnalysisTests extends RegexParserTests with Parser {

    import org.kiama.util.{Message, Messaging}
    import Syntax.{Expr, Iswim}

    def runSemanticChecks (ast : Iswim, semanticallyCorrect : Boolean,
                           messages : (Int, Message)*) : SemanticAnalysis = {
        initTree (ast)
        val messaging = new Messaging
        val analysis = new SemanticAnalysis (messaging)
        val correct = analysis.isSemanticallyCorrect (ast)
        assert (correct === semanticallyCorrect)
        assertMessages (messaging, messages : _*)
        analysis
    }

    def assertRunCheck (str : String, parser : Parser[Iswim], correct : Boolean,
                        messages : (Int, Message)*) {
        assertParseCheck (str, parser) {
            iswim =>
                runSemanticChecks (iswim, correct, messages : _*)
        }
    }

    test("simple test of use of a correctly bound variable") {
        assertRunCheck("let x = 1 in x + x", expr, true)
    }

    test("simple test of a recursive binding") {
        assertRunCheck("letrec f = fun(x){ g x } and g = fun(y){ f y } in f 1", expr, true)
    }

    test("test of top level bindings in which all variables correctly bound") {
        assertRunCheck(
""" let x = 1 and y = 60;

    let f = fun(x) { x + y };

    letrec g = fun(z) { if (z <= 0) 1 else x * g(z-1) };

    let main = fun(w) {
        f(20) + g(y)
    }
""", parser, true)
    }

    test("simple test of use of an unbound variable in body of let") {
        assertRunCheck("let x = 1 in x + y", expr, false,
            (0, Message (1, 18, "unbound variable 'y'")))
    }

    test("use of an unbound variable in an expression being bound to a variable in a let") {
        assertRunCheck("let x = y and z = x in z + x", expr, false,
            (0, Message (1, 9, "unbound variable 'y'")),
            (1, Message (1, 19, "unbound variable 'x'")))
    }

    test("test of top level bindings in which some variables incorrectly bound") {
        assertRunCheck(
""" let x = 1 and y = 60;

    let f = fun(x) { x + w };

    let w = (let z = (let y = w in y + t) in (let m = t * z in (m + z1, z)));

    letrec g = fun(z) { if (z <= 0) 1 else x * g(z-1) * (k w) }
    and k = fun(z) { g(1) + m(2) };

    let main = fun(w) {
        f(20) + g(y)
    }
""", parser, false,
            (0, Message (3, 26, "unbound variable 'w'")),
            (1, Message (5, 31, "unbound variable 'w'")),
            (2, Message (5, 40, "unbound variable 't'")),
            (3, Message (5, 55, "unbound variable 't'")),
            (4, Message (5, 69, "unbound variable 'z1'")),
            (5, Message (8, 29, "unbound variable 'm'")))
    }

    test("correct use of bound variables in a match expression") {
        assertRunCheck("""
    (1,2) match {
        ()      -> 34;
        (y,z)   -> y + z;
        x       -> x * x
    }
""", expr, true)
    }

    test("unbound variables in a match expression") {
        assertRunCheck("""
    (1,2) match {
        ()      -> y;
        (y,z)   -> y + w;
        x       -> x * z
    }
""", expr, false,
            (0, Message (3, 20, "unbound variable 'y'")),
            (1, Message (4, 24, "unbound variable 'w'")),
            (2, Message (5, 24, "unbound variable 'z'")))
    }

    test("unreachable clauses in a match expression") {
        assertRunCheck("""
    (1,2) match {
        ()      -> 1;
        w       -> w + 1;
        (y,z)   -> y + z;
        x       -> x / 10
    }
""", expr, false,
            (0, Message (5, 9, "unreachable match clause")),
            (1, Message (6, 9, "unreachable match clause")))
    }

}

