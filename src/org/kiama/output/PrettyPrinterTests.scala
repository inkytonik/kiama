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
package output

import org.junit.runner.RunWith
import org.kiama.util.Tests
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
        expect ("< : > : >") (pretty (fillsep (List (langle, colon, rangle, colon, rangle))))
    }
    
    test ("pretty-print non-empty fillsep sequence - wrap") {
        expect ("< :\n> :\n>") (pretty (group (fillsep (List (langle, colon, rangle, colon, rangle))), 3))
    } 
    
    test ("pretty-print empty fillsep sequence with sep") {
        expect ("") (pretty (fillsep (List (), comma)))
    }
        
    test ("pretty-print non-empty fillsep sequence with sep - non-wrap") {
        expect ("<, :, >, :, >") (pretty (fillsep (List (langle, colon, rangle, colon, rangle), comma)))
    }
    
    test ("pretty-print non-empty fillsep sequence with sep - wrap") {
        expect ("<, :,\n>, :,\n>") (
            pretty (group (fillsep (List (langle, colon, rangle, colon, rangle), comma)), 3)
        )
    }

    test ("pretty-print empty lsep sequence") {
        expect ("") (pretty (lsep (List (), comma)))
    }
        
    test ("pretty-print non-empty lsep sequence - non-wrap") {
        expect ("\n',\n.,\n'") (pretty (group (lsep (List (squote, dot, squote), comma)), 3))
    }

    test ("pretty-print empty lsep2 sequence") {
        expect ("") (pretty (lsep2 (List (), comma)))
    }
        
    test ("pretty-print non-empty lsep2 sequence - non-wrap") {
        expect ("'\n, .\n, '\n") (pretty (group (lsep2 (List (squote, dot, squote), comma)), 3))
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

    test ("product pretty-print empty vector") {
        expect ("Vector ()") (pretty (product (Vector ())))
    }

    test ("product pretty-print singleton vector") {
        expect ("Vector (1)") (pretty (product (Vector (1))))
    }

    test ("product pretty-print multiple-element vector") {
        expect ("Vector (1, 2, 3)") (pretty (product (Vector (1, 2, 3))))
    }        

    test ("product pretty-print empty map") {
        expect ("Map ()") (pretty (product (Map ())))
    }

    test ("product pretty-print singleton map") {
        expect ("Map (1 -> \"One\")") (pretty (product (Map (1 -> "One"))))
    }

    test ("product pretty-print multiple-element map") {
        expect ("Map (1 -> \"One\", 2 -> \"Two\", 3 -> \"Three\")") (
            pretty (product (Map (1 -> "One", 2 -> "Two", 3 -> "Three")))
        )
    }        

}

/**
 * Tests of parenthesis optimised pretty-printer module. The examples,
 * particularly Oberon0, test most cases; we just pick up the remainder
 * for coverage here.
 */
@RunWith(classOf[JUnitRunner])
class ParenPrettyPrinterTests extends Tests with PrettyPrinter with ParenPrettyPrinter {

    abstract class Exp extends PrettyExpression

    case class InOp (left : Exp, right : Exp, prio : Int, fix : Side) extends Exp with PrettyBinaryExpression {
        def priority = prio
        def fixity = Infix (fix)
        def op = "*"
    }

    case class PostOp (exp : Exp, prio : Int) extends Exp with PrettyUnaryExpression {
        def priority = prio
        def fixity = Postfix
        def op = "++"
    }

    case class PreOp (exp : Exp, prio : Int) extends Exp with PrettyUnaryExpression {
        def priority = prio
        def fixity = Prefix
        def op = "--"
    }

    case class Leaf (i : Int) extends Exp

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case Leaf (i) => value (i)
            case _        => super.toParenDoc (e)
        }

    // Postfix and prefix operators

    test ("pretty-printing a lower priority postop on the left of an left assoc infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 4), Leaf (2), 3, LeftAssoc)
        expect ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the left of an infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 2), Leaf (2), 3, LeftAssoc)
        expect ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the left of an infix uses parens") {
        val e = InOp (PreOp (Leaf (1), 4), Leaf (2), 3, LeftAssoc)
        expect ("(--1) * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the left of an infix doesn't use parens") {
        val e = InOp (PreOp (Leaf (1), 2), Leaf (2), 3, LeftAssoc)
        expect ("--1 * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the left of an infix uses parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 4), 3, LeftAssoc)
        expect ("2 * (1++)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 2), 3, LeftAssoc)
        expect ("2 * 1++") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 4), 3, LeftAssoc)
        expect ("2 * --1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 2), 3, LeftAssoc)
        expect ("2 * --1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 4), Leaf (2), 3, RightAssoc)
        expect ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 2), Leaf (2), 3, RightAssoc)
        expect ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the left of an right assoc infix uses parens") {
        val e = InOp (PreOp (Leaf (1), 4), Leaf (2), 3, RightAssoc)
        expect ("(--1) * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PreOp (Leaf (1), 2), Leaf (2), 3, RightAssoc)
        expect ("--1 * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the right of an right assoc infix uses parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 4), 3, RightAssoc)
        expect ("2 * (1++)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 2), 3, RightAssoc)
        expect ("2 * 1++") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 4), 3, RightAssoc)
        expect ("2 * --1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 2), 3, RightAssoc)
        expect ("2 * --1") (pretty (toParenDoc (e)))
    }

    // Right associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority right assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, RightAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, RightAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, LeftAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, LeftAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, LeftAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, NonAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, NonAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, NonAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    // Left associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, RightAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, RightAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, LeftAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, LeftAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, LeftAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, NonAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, NonAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, NonAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    // Non associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, RightAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, RightAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, LeftAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, LeftAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, LeftAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, NonAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, NonAssoc)
        expect ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, NonAssoc)
        expect ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    // Right associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, RightAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, RightAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, LeftAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, LeftAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, LeftAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, NonAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, NonAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, NonAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    // Left associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, RightAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, RightAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, LeftAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, LeftAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, LeftAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, NonAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, NonAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, NonAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    // Non associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, RightAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, RightAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, LeftAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, LeftAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, LeftAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, NonAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, NonAssoc)
        expect ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, NonAssoc)
        expect ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

}

