/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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

import org.kiama.util.Tests

/**
 * Basic tests of pretty-printer module.  More complex setups and some
 * combinators are tested within particular examples.
 */
class PrettyPrinterTests extends Tests with PrettyPrinter {

    test ("pretty-print empty document") {
        assertResult ("") (pretty (empty))
    }

    test ("pretty-print empty string") {
        assertResult ("") (pretty (""))
    }

    test ("pretty-print empty string via combinator") {
        assertResult ("") (pretty (string ("")))
    }

    test ("pretty-print string starting with newline") {
        assertResult ("\nthree") (pretty (string ("\nthree")))
    }

    test ("pretty-print string including newlines") {
        assertResult ("one\ntwo\nthree") (pretty (string ("one\ntwo\nthree")))
    }

    test ("pretty-print string starting with and including newlines") {
        assertResult ("\none\ntwo\nthree") (pretty (string ("\none\ntwo\nthree")))
    }

    test ("pretty-print string starting with newline - grouped") {
        assertResult (" three") (pretty (group (string ("\nthree"))))
    }

    test ("pretty-print string including newlines - grouped") {
        assertResult ("one two three") (pretty (group (string ("one\ntwo\nthree"))))
    }

    test ("pretty-print string starting with and including newlines - grouped") {
        assertResult (" one two three") (pretty (group (string ("\none\ntwo\nthree"))))
    }

    test ("pretty-print newline char") {
        assertResult ("\n") (pretty (char ('\n')))
    }

    test ("pretty-print newline char - grouped") {
        assertResult (" ") (pretty (group (char ('\n'))))
    }

    test ("pretty-print potential space line break") {
        assertResult ("\n") (pretty (line))
    }

    test ("pretty-print potential space line break - grouped") {
        assertResult (" ") (pretty (group (line)))
    }

    test ("pretty-print potential empty line break") {
        assertResult ("\n") (pretty (linebreak))
    }

    test ("pretty-print potential empty line break - grouped") {
        assertResult ("") (pretty (group (linebreak)))
    }

    {
        val linesepdoc = "a" <> line <> "b" <> line <> "c"

        test ("pretty-print space line break separators") {
            assertResult ("a\nb\nc") (pretty (linesepdoc))
        }

        test ("pretty-print space line break separators - grouped") {
            assertResult ("a b c") (pretty (group (linesepdoc)))
        }

        test ("pretty-print space line break separators - grouped, wrap") {
            assertResult ("a\nb\nc") (pretty (group (linesepdoc), 3))
        }

    }

    {
        val linesepdoc = "a" <> linebreak <> "b" <> linebreak <> "c"

        test ("pretty-print empty line break separators") {
            assertResult ("a\nb\nc") (pretty (linesepdoc))
        }

        test ("pretty-print empty line break separators - grouped") {
            assertResult ("abc") (pretty (group (linesepdoc)))
        }

        test ("pretty-print empty line break separators - grouped, wrap") {
            assertResult ("a\nb\nc") (pretty (group (linesepdoc), 3))
        }

    }

    {
        val linesepdoc = "a" <> line ("; ") <> "b" <> line ("; ") <> "c"

        test ("pretty-print semi line break separators") {
            assertResult ("a\nb\nc") (pretty (linesepdoc))
        }

        test ("pretty-print semi line break separators - grouped") {
            assertResult ("a; b; c") (pretty (group (linesepdoc)))
        }

        test ("pretty-print semi line break separators - grouped, wrap") {
            assertResult ("a\nb\nc") (pretty (group (linesepdoc), 3))
        }

    }

    test ("pretty-print no spaces") {
        assertResult ("") (pretty (spaces (0)))
    }

    test ("pretty-print non-zero spaces") {
        assertResult ("   ") (pretty (spaces (3)))
    }

    test ("pretty_any-print empty string") {
        assertResult ("\"\"") (pretty_any (""))
    }

    test ("pretty-print empty list") {
        assertResult ("List()") (pretty (Nil))
    }

    test ("pretty_any-print empty list") {
        assertResult ("Nil") (pretty_any (Nil))
    }

    test ("pretty_any-print null") {
        assertResult ("null") (pretty_any (null))
    }

    test ("pretty-print None") {
        assertResult ("None") (pretty (None))
    }

    test ("pretty_any-print None") {
        assertResult ("None") (pretty_any (None))
    }

    test ("pretty-print Some") {
        assertResult ("Some(1)") (pretty (Some (1)))
    }

    test ("pretty_any-print Some") {
        assertResult ("Some (1)") (pretty_any (Some (1)))
    }

    test ("pretty-print identifier") {
        assertResult ("hello") (pretty ("hello"))
    }

    test ("pretty_any-print identifier") {
        assertResult ("\"hello\"") (pretty_any ("hello"))
    }

    test ("pretty-print integer") {
        assertResult ("1234") (pretty (1234))
    }

    test ("pretty_any-print integer") {
        assertResult ("1234") (pretty_any (1234))
    }

    test ("pretty-print angles") {
        assertResult ("</>") (pretty (angles (forwslash)))
    }

    test ("pretty-print brackets") {
        assertResult ("[\\]") (pretty (brackets (backslash)))
    }

    test ("pretty-print squotes") {
        assertResult ("'.'") (pretty (squotes (dot)))
    }

    test ("pretty-print empty sep sequence") {
        assertResult ("") (pretty (sep (List ())))
    }

    test ("pretty-print non-empty sep sequence - non-wrap") {
        assertResult ("< : >") (pretty (sep (List (langle, colon, rangle))))
    }

    test ("pretty-print non-empty sep sequence - wrap") {
        assertResult ("<\n:\n>") (pretty (group (sep (List (langle, colon, rangle))), 2))
    }

    test ("pretty-print empty hsep sequence") {
        assertResult ("") (pretty (hsep (List ())))
    }

    test ("pretty-print non-empty hsep sequence - non-wrap") {
        assertResult ("< : >") (pretty (hsep (List (langle, colon, rangle))))
    }

    test ("pretty-print non-empty hsep sequence - wrap") {
        assertResult ("< : >") (pretty (group (hsep (List (langle, colon, rangle))), 2))
    }

    test ("pretty-print empty fillsep sequence") {
        assertResult ("") (pretty (fillsep (List ())))
    }

    test ("pretty-print non-empty fillsep sequence - non-wrap") {
        assertResult ("< : > : >") (pretty (fillsep (List (langle, colon, rangle, colon, rangle))))
    }

    test ("pretty-print non-empty fillsep sequence - wrap") {
        assertResult ("< :\n> :\n>") (pretty (group (fillsep (List (langle, colon, rangle, colon, rangle))), 3))
    }

    test ("pretty-print empty fillsep sequence with sep") {
        assertResult ("") (pretty (fillsep (List (), comma)))
    }

    test ("pretty-print non-empty fillsep sequence with sep - non-wrap") {
        assertResult ("<, :, >, :, >") (pretty (fillsep (List (langle, colon, rangle, colon, rangle), comma)))
    }

    test ("pretty-print non-empty fillsep sequence with sep - wrap") {
        assertResult ("<, :,\n>, :,\n>") (
            pretty (group (fillsep (List (langle, colon, rangle, colon, rangle), comma)), 3)
        )
    }

    test ("pretty-print empty lsep sequence") {
        assertResult ("") (pretty (lsep (List (), comma)))
    }

    test ("pretty-print non-empty lsep sequence - non-wrap") {
        assertResult ("\n',\n.,\n'") (pretty (group (lsep (List (squote, dot, squote), comma)), 3))
    }

    test ("pretty-print empty lsep2 sequence") {
        assertResult ("") (pretty (lsep2 (List (), comma)))
    }

    test ("pretty-print non-empty lsep2 sequence - non-wrap") {
        assertResult ("'\n, .\n, '\n") (pretty (group (lsep2 (List (squote, dot, squote), comma)), 3))
    }

    val l = List (lbracket, dot, equal, rbracket)

    test ("pretty-print non-empty lsep sequence - wrap") {
        assertResult ("\n[,\n.,\n=,\n]") (pretty (group (lsep (l, comma)), 3))
    }

    test ("pretty-print empty cat sequence") {
        assertResult ("") (pretty (cat (List ())))
    }

    test ("pretty-print non-empty cat sequence - non-wrap") {
        assertResult ("[.=]") (pretty (cat (l)))
    }

    test ("pretty-print non-empty cat sequence - wrap") {
        assertResult ("[\n.\n=\n]") (pretty (group (cat (l)), 3))
    }

    test ("pretty-print empty hcat sequence") {
        assertResult ("") (pretty (hcat (List ())))
    }

    test ("pretty-print non-empty hcat sequence - non-wrap") {
        assertResult ("[.=]") (pretty (hcat (l)))
    }

    test ("pretty-print non-empty hcat sequence - wrap") {
        assertResult ("[.=]") (pretty (group (hcat (l)), 3))
    }

    test ("pretty-print empty vcat sequence") {
        assertResult ("") (pretty (vcat (List ())))
    }

    test ("pretty-print non-empty vcat sequence - non-wrap") {
        assertResult ("[\n.\n=\n]") (pretty (vcat (l)))
    }

    test ("pretty-print non-empty vcat sequence - wrap") {
        assertResult ("[\n.\n=\n]") (pretty (group (vcat (l)), 3))
    }

    test ("pretty-print empty fillcat sequence") {
        assertResult ("") (pretty (fillcat (List ())))
    }

    val m = List (lbracket, dot, equal, dot, equal, dot, equal, rbracket)

    test ("pretty-print non-empty fillcat sequence - non-wrap") {
        assertResult ("[.=.=.=]") (pretty (fillcat (m)))
    }

    test ("pretty-print non-empty fillcat sequence - wrap") {
        assertResult ("[.=\n.=.\n=]") (pretty (fillcat (m), 3))
    }

    test ("pretty-print empty sterm sequence") {
        assertResult ("") (pretty (sterm (List (), colon)))
    }

    test ("pretty-print non-empty sterm sequence - non-wrap") {
        assertResult ("[:.:=:]:") (pretty (sterm (l, colon)))
    }

    test ("pretty-print non-empty sterm sequence - wrap") {
        assertResult ("[:\n.:\n=:\n]:") (pretty ((sterm (l, colon)), 3))
    }

    test ("pretty-print hanging text") {
        val words = "the hang combinator indents these words !".split (' ').toVector
        val d = hang (fillsep (words.map (text)), 3)
        assertResult ("the hang combinator\n   indents these\n   words !") (pretty (d, 15))
    }

    test ("pretty-print indented text") {
        val d = indent ("hi" <+> ("nice" <@> "world"), 2)
        assertResult ("  hi nice\n  world") (pretty (d, 5))
    }

    test ("pretty-print aligned text") {
        val d = "hi" <+> ("nice" <%> "world")
        assertResult ("hi nice\n   world") (pretty (d))
    }

    test ("pretty-print padded text") {
        val d = padto (10, "hi nice" <@> "world")
        assertResult ("hi nice\nworld     ") (pretty (d))
    }

    test ("pretty-print padded text - with linebreak") {
        val d = padtobreak (4, "hi nice") <> padtobreak (10, "world")
        assertResult ("hi nice\n    world     ") (pretty (d))
    }

    val l1 = List (1, 2, 3)

    test ("pretty-print lists of simple values - non-wrap") {
        assertResult ("List(1, 2, 3)") (pretty (list (l1)))
    }

    test ("pretty-print lists of simple values - wrap") {
        assertResult ("List(\n    1,\n    2,\n    3)") (pretty (list (l1), 3))
    }

    case class Val (i : Int)
    val l2 = List (Val (1), Val (2), Val (3))

    test ("pretty-print lists of structured values - non-wrap") {
        assertResult ("List(Val(1), Val(2), Val(3))") (pretty (list (l2)))
    }

    test ("pretty-print lists of structured values - wrap") {
        assertResult ("List(\n    Val(1),\n    Val(2),\n    Val(3))") (pretty (list (l2), 3))
    }

    class PVal (i : Int) extends PrettyPrintable {
        override def toDoc : Doc = value (i) <> text ("!")
    }
    val l3 = List (new PVal (1), new PVal (2), new PVal (3))

    test ("pretty-print lists of structured prettyy-printable values - non-wrap") {
        assertResult ("List(1!, 2!, 3!)") (pretty (plist (l3)))
    }

    test ("pretty-print lists of structured prettyy-printable values - wrap") {
        assertResult ("List(\n    1!,\n    2!,\n    3!)") (pretty (plist (l3), 3))
    }

    test ("pretty_-print empty vector") {
        assertResult ("Vector ()") (pretty_any (Vector ()))
    }

    test ("pretty_any-print singleton vector") {
        assertResult ("Vector (1)") (pretty_any (Vector (1)))
    }

    test ("pretty_any-print multiple-element vector") {
        assertResult ("Vector (1, 2, 3)") (pretty_any (Vector (1, 2, 3)))
    }

    test ("pretty_any-print empty map") {
        assertResult ("Map ()") (pretty_any (Map ()))
    }

    test ("pretty_any-print singleton map") {
        assertResult ("Map (1 -> \"One\")") (pretty_any (Map (1 -> "One")))
    }

    test ("pretty_any-print multiple-element map") {
        assertResult ("Map (1 -> \"One\", 2 -> \"Two\", 3 -> \"Three\")") (
            pretty_any (Map (1 -> "One", 2 -> "Two", 3 -> "Three"))
        )
    }

}

/**
 * Tests of parenthesis optimised pretty-printer module. The examples,
 * particularly Oberon0, test most cases; we just pick up the remainder
 * for coverage here.
 */
class ParenPrettyPrinterTests extends Tests with ParenPrettyPrinter {

    abstract class Exp extends PrettyExpression

    case class InOp (left : Exp, right : Exp, prio : Int, fix : Side) extends Exp with PrettyBinaryExpression {
        def priority : Int = prio
        def fixity : Fixity = Infix (fix)
        def op : String = "*"
    }

    case class PostOp (exp : Exp, prio : Int) extends Exp with PrettyUnaryExpression {
        def priority : Int = prio
        def fixity : Fixity = Postfix
        def op : String = "++"
    }

    case class PreOp (exp : Exp, prio : Int) extends Exp with PrettyUnaryExpression {
        def priority : Int = prio
        def fixity : Fixity = Prefix
        def op : String = "--"
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
        assertResult ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the left of an infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 2), Leaf (2), 3, LeftAssoc)
        assertResult ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the left of an infix uses parens") {
        val e = InOp (PreOp (Leaf (1), 4), Leaf (2), 3, LeftAssoc)
        assertResult ("(--1) * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the left of an infix doesn't use parens") {
        val e = InOp (PreOp (Leaf (1), 2), Leaf (2), 3, LeftAssoc)
        assertResult ("--1 * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the left of an infix uses parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 4), 3, LeftAssoc)
        assertResult ("2 * (1++)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 2), 3, LeftAssoc)
        assertResult ("2 * 1++") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 4), 3, LeftAssoc)
        assertResult ("2 * --1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 2), 3, LeftAssoc)
        assertResult ("2 * --1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 4), Leaf (2), 3, RightAssoc)
        assertResult ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 2), Leaf (2), 3, RightAssoc)
        assertResult ("1++ * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the left of an right assoc infix uses parens") {
        val e = InOp (PreOp (Leaf (1), 4), Leaf (2), 3, RightAssoc)
        assertResult ("(--1) * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PreOp (Leaf (1), 2), Leaf (2), 3, RightAssoc)
        assertResult ("--1 * 2") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the right of an right assoc infix uses parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 4), 3, RightAssoc)
        assertResult ("2 * (1++)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 2), 3, RightAssoc)
        assertResult ("2 * 1++") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 4), 3, RightAssoc)
        assertResult ("2 * --1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 2), 3, RightAssoc)
        assertResult ("2 * --1") (pretty (toParenDoc (e)))
    }

    // Right associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority right assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, RightAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, RightAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, LeftAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, LeftAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, LeftAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, NonAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, NonAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, NonAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    // Left associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, RightAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, RightAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, LeftAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, LeftAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, LeftAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, NonAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, NonAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, NonAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    // Non associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, RightAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, RightAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, LeftAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, LeftAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, LeftAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, NonAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, NonAssoc)
        assertResult ("1 * (2 * 3)") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, NonAssoc)
        assertResult ("1 * 2 * 3") (pretty (toParenDoc (e)))
    }

    // Right associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, RightAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, RightAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, LeftAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, LeftAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, LeftAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, NonAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, NonAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, NonAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    // Left associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, RightAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, RightAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, LeftAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, LeftAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, LeftAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, NonAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, NonAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, NonAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    // Non associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, RightAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, RightAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, LeftAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, LeftAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, LeftAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, NonAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, NonAssoc)
        assertResult ("(2 * 3) * 1") (pretty (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, NonAssoc)
        assertResult ("2 * 3 * 1") (pretty (toParenDoc (e)))
    }

}

