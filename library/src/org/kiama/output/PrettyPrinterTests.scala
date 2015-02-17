/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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
class PrettyPrinterTests extends org.kiama.util.PrettyPrinterTests with PrettyPrinter {

    test ("pretty-print empty document") {
        assertResult ("") (layout (empty))
    }

    test ("pretty-print empty string") {
        assertResult ("") (layout (""))
    }

    test ("pretty-print empty string via combinator") {
        assertResult ("") (layout (string ("")))
    }

    test ("pretty-print string starting with newline") {
        assertResult ("\nthree") (layout (string ("\nthree")))
    }

    test ("pretty-print string including newlines") {
        assertResult ("one\ntwo\nthree") (layout (string ("one\ntwo\nthree")))
    }

    test ("pretty-print string starting with and including newlines") {
        assertResult ("\none\ntwo\nthree") (layout (string ("\none\ntwo\nthree")))
    }

    test ("pretty-print string starting with newline - grouped") {
        assertResult (" three") (layout (group (string ("\nthree"))))
    }

    test ("pretty-print string including newlines - grouped") {
        assertResult ("one two three") (layout (group (string ("one\ntwo\nthree"))))
    }

    test ("pretty-print string starting with and including newlines - grouped") {
        assertResult (" one two three") (layout (group (string ("\none\ntwo\nthree"))))
    }

    test ("pretty-print newline char") {
        assertResult ("\n") (layout (char ('\n')))
    }

    test ("pretty-print newline char - grouped") {
        assertResult (" ") (layout (group (char ('\n'))))
    }

    test ("pretty-print potential space line break") {
        assertResult ("\n") (layout (line))
    }

    test ("pretty-print potential space line break - grouped") {
        assertResult (" ") (layout (group (line)))
    }

    test ("pretty-print potential empty line break") {
        assertResult ("\n") (layout (linebreak))
    }

    test ("pretty-print potential empty line break - grouped") {
        assertResult ("") (layout (group (linebreak)))
    }

    {
        val linesepdoc = "a" <> line <> "b" <> line <> "c"

        test ("pretty-print space line break separators") {
            assertResult ("a\nb\nc") (layout (linesepdoc))
        }

        test ("pretty-print space line break separators - grouped") {
            assertResult ("a b c") (layout (group (linesepdoc)))
        }

        test ("pretty-print space line break separators - grouped, wrap") {
            assertResult ("a\nb\nc") (layout (group (linesepdoc), 3))
        }

    }

    {
        val linesepdoc = "a" <> linebreak <> "b" <> linebreak <> "c"

        test ("pretty-print empty line break separators") {
            assertResult ("a\nb\nc") (layout (linesepdoc))
        }

        test ("pretty-print empty line break separators - grouped") {
            assertResult ("abc") (layout (group (linesepdoc)))
        }

        test ("pretty-print empty line break separators - grouped, wrap") {
            assertResult ("a\nb\nc") (layout (group (linesepdoc), 2))
        }

    }

    {
        val linesepdoc = "a" <> line ("; ") <> "b" <> line ("; ") <> "c"

        test ("pretty-print semi line break separators") {
            assertResult ("a\nb\nc") (layout (linesepdoc))
        }

        test ("pretty-print semi line break separators - grouped") {
            assertResult ("a; b; c") (layout (group (linesepdoc)))
        }

        test ("pretty-print semi line break separators - grouped, wrap") {
            assertResult ("a\nb\nc") (layout (group (linesepdoc), 3))
        }

    }

    test ("pretty-print no spaces") {
        assertResult ("") (layout (spaces (0)))
    }

    test ("pretty-print non-zero spaces") {
        assertResult ("   ") (layout (spaces (3)))
    }

    test ("pretty any-print empty string") {
        assertResult ("\"\"") (layout (any ("")))
    }

    test ("pretty any-print empty list") {
        assertResult ("Nil") (layout (any (Nil)))
    }

    test ("pretty any-print null") {
        assertResult ("null") (layout (any (null)))
    }

    test ("pretty any-print None") {
        assertResult ("None") (layout (any (None)))
    }

    test ("pretty any-print Some") {
        assertResult ("Some (1)") (layout (any (Some (1))))
    }

    test ("pretty-print identifier") {
        assertResult ("hello") (layout ("hello"))
    }

    test ("pretty any-print identifier") {
        assertResult ("\"hello\"") (layout (any ("hello")))
    }

    test ("pretty any-print integer") {
        assertResult ("1234") (layout (any (1234)))
    }

    test ("pretty-print angles") {
        assertResult ("</>") (layout (angles (forwslash)))
    }

    test ("pretty-print brackets") {
        assertResult ("[\\]") (layout (brackets (backslash)))
    }

    test ("pretty-print squotes") {
        assertResult ("'.'") (layout (squotes (dot)))
    }

    test ("pretty-print empty sep sequence") {
        assertResult ("") (layout (sep (List ())))
    }

    test ("pretty-print non-empty sep sequence - non-wrap") {
        assertResult ("< : >") (layout (sep (List (langle, colon, rangle))))
    }

    test ("pretty-print non-empty sep sequence - wrap") {
        assertResult ("<\n:\n>") (layout (group (sep (List (langle, colon, rangle))), 2))
    }

    test ("pretty-print empty hsep sequence") {
        assertResult ("") (layout (hsep (List ())))
    }

    test ("pretty-print non-empty hsep sequence - non-wrap") {
        assertResult ("< : >") (layout (hsep (List (langle, colon, rangle))))
    }

    test ("pretty-print non-empty hsep sequence - wrap") {
        assertResult ("< : >") (layout (group (hsep (List (langle, colon, rangle))), 2))
    }

    test ("pretty-print empty fillsep sequence") {
        assertResult ("") (layout (fillsep (List ())))
    }

    test ("pretty-print non-empty fillsep sequence - non-wrap") {
        assertResult ("< : > : >") (layout (fillsep (List (langle, colon, rangle, colon, rangle))))
    }

    test ("pretty-print non-empty fillsep sequence - wrap") {
        assertResult ("< :\n> :\n>") (layout (group (fillsep (List (langle, colon, rangle, colon, rangle))), 3))
    }

    test ("pretty-print empty fillsep sequence with sep") {
        assertResult ("") (layout (fillsep (List (), comma)))
    }

    test ("pretty-print non-empty fillsep sequence with sep - non-wrap") {
        assertResult ("<, :, >, :, >") (layout (fillsep (List (langle, colon, rangle, colon, rangle), comma)))
    }

    test ("pretty-print non-empty fillsep sequence with sep - wrap") {
        assertResult ("<, :,\n>, :,\n>") (
            layout (group (fillsep (List (langle, colon, rangle, colon, rangle), comma)), 3)
        )
    }

    test ("pretty-print empty lsep sequence") {
        assertResult ("") (layout (lsep (List (), comma)))
    }

    test ("pretty-print non-empty lsep sequence - non-wrap") {
        assertResult ("\n',\n.,\n'") (layout (group (lsep (List (squote, dot, squote), comma)), 3))
    }

    test ("pretty-print empty lsep2 sequence") {
        assertResult ("") (layout (lsep2 (List (), comma)))
    }

    test ("pretty-print non-empty lsep2 sequence - non-wrap") {
        assertResult ("'\n, .\n, '\n") (layout (group (lsep2 (List (squote, dot, squote), comma)), 3))
    }

    val l = List (langle, dot, equal, rangle)

    test ("pretty-print non-empty lsep sequence - wrap") {
        assertResult ("\n<,\n.,\n=,\n>") (layout (group (lsep (l, comma)), 3))
    }

    test ("pretty-print empty cat sequence") {
        assertResult ("") (layout (cat (List ())))
    }

    test ("pretty-print non-empty cat sequence - non-wrap") {
        assertResult ("<.=>") (layout (cat (l)))
    }

    test ("pretty-print non-empty cat sequence - wrap") {
        assertResult ("<\n.\n=\n>") (layout (group (cat (l)), 3))
    }

    test ("pretty-print empty hcat sequence") {
        assertResult ("") (layout (hcat (List ())))
    }

    test ("pretty-print non-empty hcat sequence - non-wrap") {
        assertResult ("<.=>") (layout (hcat (l)))
    }

    test ("pretty-print non-empty hcat sequence - wrap") {
        assertResult ("<.=>") (layout (group (hcat (l)), 3))
    }

    test ("pretty-print empty vcat sequence") {
        assertResult ("") (layout (vcat (List ())))
    }

    test ("pretty-print non-empty vcat sequence - non-wrap") {
        assertResult ("<\n.\n=\n>") (layout (vcat (l)))
    }

    test ("pretty-print non-empty vcat sequence - wrap") {
        assertResult ("<\n.\n=\n>") (layout (group (vcat (l)), 3))
    }

    test ("pretty-print empty fillcat sequence") {
        assertResult ("") (layout (fillcat (List ())))
    }

    val m = List (langle, dot, equal, dot, equal, dot, equal, rangle)

    test ("pretty-print non-empty fillcat sequence - non-wrap") {
        assertResult ("<.=.=.=>") (layout (fillcat (m)))
    }

    test ("pretty-print non-empty fillcat sequence - wrap") {
        assertResult ("<.=\n.=.\n=>") (layout (fillcat (m), 3))
    }

    test ("pretty-print empty sterm sequence") {
        assertResult ("") (layout (sterm (List (), colon)))
    }

    test ("pretty-print non-empty sterm sequence - non-wrap") {
        assertResult ("<:.:=:>:") (layout (sterm (l, colon)))
    }

    test ("pretty-print non-empty sterm sequence - wrap") {
        assertResult ("<:\n.:\n=:\n>:") (layout ((sterm (l, colon)), 3))
    }

    test ("pretty-print hanging text") {
        val words = "the hang combinator indents these words !".split (' ').toVector
        val d = hang (fillsep (words.map (text)), 3)
        assertResult ("the hang combinator\n   indents these\n   words !") (layout (d, 15))
    }

    test ("pretty-print indented text") {
        val d = indent ("hi" <+> ("nice" <@> "world"), 2)
        assertResult ("  hi nice\n  world") (layout (d, 5))
    }

    test ("pretty-print aligned text") {
        val d = "hi" <+> ("nice" <%> "world")
        assertResult ("hi nice\n   world") (layout (d))
    }

    test ("pretty-print padded text") {
        val d = padto (10, "hi nice" <@> "world")
        assertResult ("hi nice\nworld     ") (layout (d))
    }

    test ("pretty-print padded text - with linebreak") {
        val d = padtobreak (4, "hi nice") <> padtobreak (10, "world")
        assertResult ("hi nice\n    world     ") (layout (d))
    }

    val l1 = List (1, 2, 3)
    val l2 = List ('a', 'b')

    test ("pretty-print lists of simple values - non-wrap") {
        assertResult ("List (1, 2, 3)") (layout (list (l1)))
    }

    test ("pretty-print simple value arguments - non-wrap") {
        assertResult ("(a, b)") (layout (arguments (l2)))
    }

    test ("pretty-print lists of simple values - wrap") {
        assertResult ("List (\n    a,\n    b)") (layout (list (l2), 3))
    }

    test ("pretty-print simple value arguments - wrap") {
        assertResult ("(\n    1,\n    2,\n    3)") (layout (arguments (l1), 3))
    }

    test ("pretty-print lists of simple values - wrap, non-default") {
        assertResult ("Foo (\n    +;\n    +;\n    +;)") (
            layout (list (l1, "Foo", (_ : Int) => plus, semi, lterm), 3)
        )
    }

    test ("pretty-print simple value arguments  - wrap, non-default") {
        assertResult ("(=\n    . =\n    )") (
            layout (arguments (l2, (_ : Char) => equal, dot, lsep2), 3)
        )
    }

    test ("pretty-print sequences of simple values - non-wrap") {
        assertResult ("Seq (1, 2, 3)") (layout (seq (l1)))
    }

    test ("pretty-print sequences of simple values - wrap") {
        assertResult ("Seq (\n    1,\n    2,\n    3)") (layout (seq (l1), 3))
    }

    case class Val (i : Int)
    val l3 = List (Val (1), Val (2), Val (3))

    test ("pretty-print lists of structured values - non-wrap") {
        assertResult ("List (Val(1), Val(2), Val(3))") (layout (list (l3)))
    }

    test ("pretty-print lists of structured values - wrap") {
        assertResult ("List (\n    Val(1),\n    Val(2),\n    Val(3))") (layout (list (l3), 3))
    }

    test ("pretty-print sequences of structured values - non-wrap") {
        assertResult ("Seq (Val(1), Val(2), Val(3))") (layout (seq (l3)))
    }

    test ("pretty-print sequences of structured values - wrap") {
        assertResult ("Seq (\n    Val(1),\n    Val(2),\n    Val(3))") (layout (seq (l3), 3))
    }

    test ("pretty any-print empty vector") {
        assertResult ("Vector ()") (layout (any (Vector ())))
    }

    test ("pretty any-print singleton vector") {
        assertResult ("Vector (1)") (layout (any (Vector (1))))
    }

    test ("pretty any-print multiple-element vector") {
        assertResult ("Vector (1, 2, 3)") (layout (any (Vector (1, 2, 3))))
    }

    test ("pretty any-print empty map") {
        assertResult ("Map ()") (layout (any (Map ())))
    }

    test ("pretty any-print singleton map") {
        assertResult ("Map (1 -> \"One\")") (layout (any (Map (1 -> "One"))))
    }

    test ("pretty any-print multiple-element map") {
        assertResult ("Map (1 -> \"One\", 2 -> \"Two\", 3 -> \"Three\")") (
            layout (any (Map (1 -> "One", 2 -> "Two", 3 -> "Three")))
        )
    }

    // Position map

    test ("pretty-printing a doc with no positioned nodes yields an empty position map") {
        val d = indent ("hi" <+> ("nice" <@> "world"), 2)
        assertPositions (Map.empty) (pretty (d))
    }

    test ("pretty any-print multiple-element map yields correct position map") {

        // Map (1 -> "One", 2 -> "Two", 3 -> "Three")

        val map = Map (1 -> "One", 2 -> "Two", 3 -> "Three")
        assertPositions (
            Map (
                map -> Range (0, 43),
                (1, "One") -> Range (5, 16),
                (2, "Two") -> Range (17, 28),
                (3, "Three") -> Range (29, 42),
                1 -> Range (5, 7),
                2 -> Range (17, 19),
                3 -> Range (29, 31),
                "One" -> Range (10, 16),
                "Two" -> Range (22, 28),
                "Three" -> Range (34, 42)
            )
        ) (pretty (any (map)))

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
        assertResult ("1++ * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the left of an infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 2), Leaf (2), 3, LeftAssoc)
        assertResult ("1++ * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the left of an infix uses parens") {
        val e = InOp (PreOp (Leaf (1), 4), Leaf (2), 3, LeftAssoc)
        assertResult ("(--1) * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the left of an infix doesn't use parens") {
        val e = InOp (PreOp (Leaf (1), 2), Leaf (2), 3, LeftAssoc)
        assertResult ("--1 * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the left of an infix uses parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 4), 3, LeftAssoc)
        assertResult ("2 * (1++)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 2), 3, LeftAssoc)
        assertResult ("2 * 1++") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 4), 3, LeftAssoc)
        assertResult ("2 * --1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the right of an infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 2), 3, LeftAssoc)
        assertResult ("2 * --1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 4), Leaf (2), 3, RightAssoc)
        assertResult ("1++ * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PostOp (Leaf (1), 2), Leaf (2), 3, RightAssoc)
        assertResult ("1++ * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the left of an right assoc infix uses parens") {
        val e = InOp (PreOp (Leaf (1), 4), Leaf (2), 3, RightAssoc)
        assertResult ("(--1) * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the left of an right assoc infix doesn't use parens") {
        val e = InOp (PreOp (Leaf (1), 2), Leaf (2), 3, RightAssoc)
        assertResult ("--1 * 2") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority postop on the right of an right assoc infix uses parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 4), 3, RightAssoc)
        assertResult ("2 * (1++)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority postop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PostOp (Leaf (1), 2), 3, RightAssoc)
        assertResult ("2 * 1++") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower priority preop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 4), 3, RightAssoc)
        assertResult ("2 * --1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher priority preop on the right of an right assoc infix doesn't use parens") {
        val e = InOp (Leaf (2), PreOp (Leaf (1), 2), 3, RightAssoc)
        assertResult ("2 * --1") (layout (toParenDoc (e)))
    }

    // Right associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority right assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, RightAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, RightAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, LeftAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, LeftAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, LeftAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, NonAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 3, NonAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 4, NonAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    // Left associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, RightAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, RightAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, LeftAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, LeftAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, LeftAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, NonAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, NonAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, NonAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    // Non associative infix operator on right of other infix operators

    test ("pretty-printing a lower-priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, RightAssoc), 2, RightAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, RightAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, RightAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, LeftAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, LeftAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, LeftAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 2, NonAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 3, NonAssoc)
        assertResult ("1 * (2 * 3)") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = InOp (Leaf (1), InOp (Leaf (2), Leaf (3), 3, LeftAssoc), 4, NonAssoc)
        assertResult ("1 * 2 * 3") (layout (toParenDoc (e)))
    }

    // Right associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, RightAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, RightAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, LeftAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, LeftAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, LeftAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, NonAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 3, NonAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority right assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 4, NonAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    // Left associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, RightAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, RightAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, LeftAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, LeftAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, LeftAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, NonAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, NonAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority left assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, NonAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    // Non associative infix operator on left of other infix operators

    test ("pretty-printing a lower-priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, RightAssoc), Leaf (1), 2, RightAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, RightAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, RightAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the left of a left assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, LeftAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, LeftAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, LeftAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a lower-priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 2, NonAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing an equal priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 3, NonAssoc)
        assertResult ("(2 * 3) * 1") (layout (toParenDoc (e)))
    }

    test ("pretty-printing a higher-priority non assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = InOp (InOp (Leaf (2), Leaf (3), 3, LeftAssoc), Leaf (1), 4, NonAssoc)
        assertResult ("2 * 3 * 1") (layout (toParenDoc (e)))
    }

}

