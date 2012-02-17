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
package util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Basic tests of pretty-printer module.  More complex setups and some
 * combinators are tested within particular examples.
 */
@RunWith(classOf[JUnitRunner])
class PrettyPrinterTests extends Tests with PrettyPrinter {

    test ("pretty-print empty document") {
        expect ("") (pretty (empty))
    }
    
    test ("pretty-print empty string") {
        expect ("") (pretty (""))
    }
    
    test ("pretty-print empty string via combinator") {
        expect ("") (pretty (string ("")))
    }
    
    test ("pretty-print string starting with newline") {
        expect ("\nthree") (pretty (string ("\nthree")))
    }
    
    test ("pretty-print string including newlines") {
        expect ("one\ntwo\nthree") (pretty (string ("one\ntwo\nthree")))
    }
    
    test ("pretty-print string starting with and including newlines") {
        expect ("\none\ntwo\nthree") (pretty (string ("\none\ntwo\nthree")))
    }
    
    test ("pretty-print string starting with newline - grouped") {
        expect (" three") (pretty (group (string ("\nthree"))))
    }
    
    test ("pretty-print string including newlines - grouped") {
        expect ("one two three") (pretty (group (string ("one\ntwo\nthree"))))
    }
    
    test ("pretty-print string starting with and including newlines - grouped") {
        expect (" one two three") (pretty (group (string ("\none\ntwo\nthree"))))
    }
    
    test ("pretty-print newline char") {
        expect ("\n") (pretty (char ('\n')))
    }
    
    test ("pretty-print newline char - grouped") {
        expect (" ") (pretty (group (char ('\n'))))
    }
    
    test ("pretty-print no spaces") {
        expect ("") (pretty (spaces (0)))
    }
    
    test ("pretty-print non-zero spaces") {
        expect ("   ") (pretty (spaces (3)))
    }
    
    test ("pretty-print empty string - product") {
        expect ("\"\"") (pretty (product ("")))
    }
    
    test ("pretty-print empty list") {
        expect ("List()") (pretty (Nil))
    }
    
    test ("pretty-print empty list product") {
        expect ("Nil") (pretty (product (Nil)))
    }
    
    test ("pretty-print identifier") {
        expect ("hello") (pretty ("hello"))
    }
    
    test ("pretty-print identifier - product") {
        expect ("\"hello\"") (pretty (product ("hello")))
    }
    
    test ("pretty-print integer") {
        expect ("1234") (pretty (1234))
    }
    
    test ("pretty-print integer - product") {
        expect ("1234") (pretty (product (1234)))
    }
    
    test ("pretty-print angles") {
        expect ("</>") (pretty (angles (forwslash)))
    }
    
    test ("pretty-print brackets") {
        expect ("[\\]") (pretty (brackets (backslash)))
    }
    
    test ("pretty-print squotes") {
        expect ("'.'") (pretty (squotes (dot)))
    }
    
    test ("pretty-print empty sep sequence") {
        expect ("") (pretty (sep (List ())))
    }
    
    test ("pretty-print non-empty sep sequence - non-wrap") {
        expect ("< : >") (pretty (sep (List (langle, colon, rangle))))
    }
    
    test ("pretty-print non-empty sep sequence - wrap") {
        expect ("<\n:\n>") (pretty (group (sep (List (langle, colon, rangle))), 2))
    }
    
    test ("pretty-print empty hsep sequence") {
        expect ("") (pretty (hsep (List ())))
    }
    
    test ("pretty-print non-empty hsep sequence - non-wrap") {
        expect ("< : >") (pretty (hsep (List (langle, colon, rangle))))
    }
    
    test ("pretty-print non-empty hsep sequence - wrap") {
        expect ("< : >") (pretty (group (hsep (List (langle, colon, rangle))), 2))
    }
    
    test ("pretty-print empty fillsep sequence") {
        expect ("") (pretty (fillsep (List ())))
    }
        
    test ("pretty-print non-empty fillsep sequence - non-wrap") {
        expect ("< : >") (pretty (fillsep (List (langle, colon, rangle))))
    }
    
    test ("pretty-print non-empty fillsep sequence - wrap") {
        expect ("< :\n>") (pretty (group (fillsep (List (langle, colon, rangle))), 3))
    }
    
    test ("pretty-print empty lsep sequence") {
        expect ("") (pretty (lsep (List (), comma)))
    }
        
    test ("pretty-print non-empty lsep sequence - non-wrap") {
        expect ("', ., '") (pretty (group (lsep (List (squote, dot, squote), comma))))
    }
    
    val l = List (lbracket, dot, equal, rbracket)
    
    test ("pretty-print non-empty lsep sequence - wrap") {
        expect ("\n[,\n.,\n=,\n]") (pretty (group (lsep (l, comma)), 3))
    }
    
    test ("pretty-print empty cat sequence") {
        expect ("") (pretty (cat (List ())))
    }
        
    test ("pretty-print non-empty cat sequence - non-wrap") {
        expect ("[.=]") (pretty (cat (l)))
    }
    
    test ("pretty-print non-empty cat sequence - wrap") {
        expect ("[\n.\n=\n]") (pretty (group (cat (l)), 3))
    }
    
    test ("pretty-print empty hcat sequence") {
        expect ("") (pretty (hcat (List ())))
    }
        
    test ("pretty-print non-empty hcat sequence - non-wrap") {
        expect ("[.=]") (pretty (hcat (l)))
    }
    
    test ("pretty-print non-empty hcat sequence - wrap") {
        expect ("[.=]") (pretty (group (hcat (l)), 3))
    }
    
    test ("pretty-print empty vcat sequence") {
        expect ("") (pretty (vcat (List ())))
    }
        
    test ("pretty-print non-empty vcat sequence - non-wrap") {
        expect ("[\n.\n=\n]") (pretty (vcat (l)))
    }
    
    test ("pretty-print non-empty vcat sequence - wrap") {
        expect ("[\n.\n=\n]") (pretty (group (vcat (l)), 3))
    }
    
    test ("pretty-print empty fillcat sequence") {
        expect ("") (pretty (fillcat (List ())))
    }
    
    val m = List (lbracket, dot, equal, dot, equal, dot, equal, rbracket)
        
    test ("pretty-print non-empty fillcat sequence - non-wrap") {
        expect ("[.=.=.=]") (pretty (fillcat (m)))
    }
    
    test ("pretty-print non-empty fillcat sequence - wrap") {
        expect ("[.=\n.=.\n=]") (pretty (fillcat (m), 3))
    }
    
    test ("pretty-print empty sterm sequence") {
        expect ("") (pretty (sterm (List (), colon)))
    }
        
    test ("pretty-print non-empty sterm sequence - non-wrap") {
        expect ("[:.:=:]:") (pretty (sterm (l, colon)))
    }
    
    test ("pretty-print non-empty sterm sequence - wrap") {
        expect ("[:\n.:\n=:\n]:") (pretty ((sterm (l, colon)), 3))
    }
    
    val l1 = List (1, 2, 3)

    test ("pretty-print lists of simple values - non-wrap") {
        expect ("List(1, 2, 3)") (pretty (list (l1)))
    }
    
    test ("pretty-print lists of simple values - wrap") {
        expect ("List(\n    1,\n    2,\n    3)") (pretty (list (l1), 3))
    }
    
    case class Val (i : Int)
    val l2 = List (Val (1), Val (2), Val (3))

    test ("pretty-print lists of structured values - non-wrap") {
        expect ("List(Val(1), Val(2), Val(3))") (pretty (list (l2)))
    }

    test ("pretty-print lists of structured values - wrap") {
        expect ("List(\n    Val(1),\n    Val(2),\n    Val(3))") (pretty (list (l2), 3))
    }

     class PVal (i : Int) extends PrettyPrintable {
         override def toDoc : Doc = value (i) <> text ("!")
     }
     val l3 = List (new PVal (1), new PVal (2), new PVal (3))
     
     test ("pretty-print lists of structured prettyy-printable values - non-wrap") {
         expect ("List(1!, 2!, 3!)") (pretty (plist (l3)))
     }
     
     test ("pretty-print lists of structured prettyy-printable values - wrap") {
         expect ("List(\n    1!,\n    2!,\n    3!)") (pretty (plist (l3), 3))
     }

}
