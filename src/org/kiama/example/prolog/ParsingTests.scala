/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package example.prolog

import org.junit.runner.RunWith
import org.kiama.util.RegexParserTests
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class ParsingTests extends SyntaxAnalysis with RegexParserTests {

    import PrologTree._

    test ("parsing an atom as an atom works") {
        assertParseOk ("albert", atom, "albert")
    }

    test ("parsing a non-atom as an atom gives an error") {
        assertParseError ("X", atom, 1, 1, "string matching regex `[a-z][a-zA-Z]*' expected but `X' found")
    }

    test ("parsing an atomic literal produces the correct tree") {
        assertParseOk ("albert", literal, Atom ("albert"))
    }

    test ("parsing a predicate literal produces the correct tree") {
        assertParseOk ("likes(X,nigel)", literal,
            Pred ("likes", List (Var ("X"), Atom ("nigel"))))
    }

    // Additional tests:
    
    // More atom tests
    
    test ("parsing a single letter atom as an atom works") {
        assertParseOk ("x", atom, "x")
    }
    
    // Variable tests 

    test ("parsing an atom as an var gives an error") {
        assertParseError ("albert", varr, 1, 1, "string matching regex `[A-Z][a-zA-Z]*' expected but `a' found")
    }

    test ("parsing a single letter var as a var works") {
        assertParseOk ("X", varr, "X")
    }

    test ("parsing a length > 1 var as a var works") {
        assertParseOk ("XYZ", varr, "XYZ")
    }

    test ("parsing a mixed-case var as a var works") {
        assertParseOk ("XxyyABc", varr, "XxyyABc")
    }

    // Program tests
    
    test ("parsing an empty program gives an error") {
        assertParseError ("", program, 1, 1, "string matching regex `[a-z][a-zA-Z]*' expected but end of source found")
    }

    test ("parsing a single clause works") {
        assertParseOk ("female(mary).", program,
            Program (List (Fact (Pred ("female", List (Atom ("mary")))))))
    }

    test ("parsing multiple clauses works") {
        assertParseOk ("female(mary).\nmale (john).\nmale (luke).", program,
            Program (List (Fact (Pred ("female", List (Atom ("mary")))),
                           Fact (Pred ("male", List (Atom ("john")))),
                           Fact (Pred ("male", List (Atom ("luke")))))))
    }

    // Clause tests
    
    test ("parsing a rule works") {
        assertParseOk ("likes(john,X) :- likes(X,wine), likes(X,food).", clause,
            Rule (Pred ("likes", List (Atom ("john"), Var ("X"))),
                  List (Pred ("likes", List (Var ("X"), Atom ("wine"))),
                        Pred ("likes", List (Var ("X"), Atom ("food"))))))
    }
    
    test ("parsing a fact works") {
        assertParseOk ("bodgie (boo).", clause,
            Fact (Pred ("bodgie", List (Atom ("boo")))))
    }

    // Literal tests

    test ("parsing an atomic literal works") {
        assertParseOk ("roger", literal, Atom ("roger"))
    }

    test ("parsing a variable as a literal fails") {
        assertParseError ("Vavoom", literal, 1, 1, "string matching regex `[a-z][a-zA-Z]*' expected but `V' found")
    }

    test ("parsing a predicate literal with zero arguments fails") {
        assertParseError ("likes ()", literal, 1, 8, "`[' expected but `)' found")
    }

    test ("parsing a predicate literal with one argument works") {
        assertParseOk ("likes (X)", literal,
            Pred ("likes", List (Var ("X"))))
    }

    test ("parsing a predicate literal with many argument works") {
        assertParseOk ("likes (X, at, VAR)", literal,
            Pred ("likes", List (Var ("X"), Atom ("at"), Var ("VAR"))))
    }

    test ("parsing a predicate literal with a predicate argument works") {
        assertParseOk ("likes (X, likes (Y, Z), W)", literal,
            Pred ("likes", List (Var ("X"),
                                 Pred ("likes", List (Var ("Y"), Var ("Z"))),
                                 Var ("W"))))
    }
        
    test ("parsing a cut works") {
        assertParseOk ("!", cut, Cut ())
    }
    
    test ("parsing a literal list containg a cut works") {
        assertParseOk ("likes (X), !, male (Y)", literals,
            List (Pred ("likes", List (Var ("X"))),
                  Cut (), 
                  Pred ("male", List (Var ("Y")))))
    }
    
    test ("parsing a nested cut fails") {
        assertParseError ("likes (!)", literal, 1, 8, "`[' expected but `!' found")
    }

    // Literal list tests, assuming that literal tests take care of most cases
    // for the components

    test ("parsing an empty literal list gives an error") {
        assertParseError ("", literals, 1, 1, "`!' expected but end of source found")
    }

    test ("parsing a singleton literal list works") {
        assertParseOk ("nonny (harold)", literals,
            List (Pred ("nonny", List (Atom ("harold")))))
    }

    test ("parsing multiple literal list works") {
        assertParseOk ("nonny (harold), ninny (tony), nanny (jane)", literals,
            List (Pred ("nonny", List (Atom ("harold"))),
                  Pred ("ninny", List (Atom ("tony"))),
                  Pred ("nanny", List (Atom ("jane")))))
    }

    // Integer tests

    test ("parsing a single digit integer works") {
        assertParseOk ("0", integer, Integer (0))
        assertParseOk ("4", integer, Integer (4))
        assertParseOk ("7", integer, Integer (7))
        assertParseOk ("9", integer, Integer (9))
    }
    
    test ("parsing a non-trivial integer works") {
        assertParseOk ("78", integer, Integer (78))
        assertParseOk ("123", integer, Integer (123))
        assertParseOk ("793223", integer, Integer (793223))
    }
    
    test ("parsing non-integers as integers fails") {
        assertParseError ("x", integer, 1, 1,
            "string matching regex `[0-9]+' expected but `x' found")
        assertParseError ("Eugene", integer, 1, 1,
            "string matching regex `[0-9]+' expected but `E' found")
        assertParseError ("(", integer, 1, 1,
            "string matching regex `[0-9]+' expected but `(' found")
    }

    // List terms
    
    test ("parsing an empty list works") {
        assertParseOk ("[]", list, Pred ("nil", Nil))
    }
    
    test ("parsing a singleton list works") {
        assertParseOk ("[a]", list, Pred ("cons", List (Atom ("a"), Pred ("nil", Nil))))
    }
    
    test ("parsing a muliple-element list works") {
        assertParseOk ("[a,b,c]", list,
            Pred ("cons", List (Atom ("a"),
                Pred ("cons", List (Atom ("b"),
                    Pred ("cons", List (Atom ("c"),
                        Pred ("nil" , Nil))))))))
    }

}
