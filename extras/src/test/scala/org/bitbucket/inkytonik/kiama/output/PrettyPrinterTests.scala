/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package output

import org.bitbucket.inkytonik.kiama.util.KiamaTests

/**
 * Basic tests of pretty-printer module.  More complex setups and some
 * combinators are tested within particular examples.
 */
class PrettyPrinterTests extends org.bitbucket.inkytonik.kiama.util.PrettyPrinterTests with PrettyPrinter {

    test("pretty-print empty document") {
        layout(emptyDoc) shouldBe ""
    }

    test("pretty-print empty string") {
        layout("") shouldBe ""
    }

    test("pretty-print empty string via combinator") {
        layout(string("")) shouldBe ""
    }

    test("pretty-print string starting with newline") {
        layout(string("\nthree")) shouldBe "\nthree"
    }

    test("pretty-print string including newlines") {
        layout(string("one\ntwo\nthree")) shouldBe "one\ntwo\nthree"
    }

    test("pretty-print string starting with and including newlines") {
        layout(string("\none\ntwo\nthree")) shouldBe "\none\ntwo\nthree"
    }

    test("pretty-print string starting with newline - grouped") {
        layout(group(string("\nthree"))) shouldBe " three"
    }

    test("pretty-print string including newlines - grouped") {
        layout(group(string("one\ntwo\nthree"))) shouldBe "one two three"
    }

    test("pretty-print string starting with and including newlines - grouped") {
        layout(group(string("\none\ntwo\nthree"))) shouldBe " one two three"
    }

    test("pretty-print newline char") {
        layout(char('\n')) shouldBe "\n"
    }

    test("pretty-print newline char - grouped") {
        layout(group(char('\n'))) shouldBe " "
    }

    test("pretty-print potential space line break") {
        layout(line) shouldBe "\n"
    }

    test("pretty-print potential space line break - grouped") {
        layout(group(line)) shouldBe " "
    }

    test("pretty-print potential empty line break") {
        layout(linebreak) shouldBe "\n"
    }

    test("pretty-print potential empty line break - grouped") {
        layout(group(linebreak)) shouldBe ""
    }

    {
        val linesepdoc = "a" <> line <> "b" <> line <> "c"

        test("pretty-print space line break separators") {
            layout(linesepdoc) shouldBe "a\nb\nc"
        }

        test("pretty-print space line break separators - grouped") {
            layout(group(linesepdoc)) shouldBe "a b c"
        }

        test("pretty-print space line break separators - grouped, wrap") {
            layout(group(linesepdoc), 3) shouldBe "a\nb\nc"
        }

    }

    {
        val linesepdoc = "a" <> linebreak <> "b" <> linebreak <> "c"

        test("pretty-print empty line break separators") {
            layout(linesepdoc) shouldBe "a\nb\nc"
        }

        test("pretty-print empty line break separators - grouped") {
            layout(group(linesepdoc)) shouldBe "abc"
        }

        test("pretty-print empty line break separators - grouped, wrap") {
            layout(group(linesepdoc), 2) shouldBe "a\nb\nc"
        }

    }

    {
        val linesepdoc = "a" <> line("; ") <> "b" <> line("; ") <> "c"

        test("pretty-print semi line break separators") {
            layout(linesepdoc) shouldBe "a\nb\nc"
        }

        test("pretty-print semi line break separators - grouped") {
            layout(group(linesepdoc)) shouldBe "a; b; c"
        }

        test("pretty-print semi line break separators - grouped, wrap") {
            layout(group(linesepdoc), 3) shouldBe "a\nb\nc"
        }

    }

    test("pretty-print no spaces") {
        layout(spaces(0)) shouldBe ""
    }

    test("pretty-print non-zero spaces") {
        layout(spaces(3)) shouldBe "   "
    }

    test("pretty any-print empty string") {
        layout(any("")) shouldBe "\"\""
    }

    test("pretty any-print empty list") {
        layout(any(Nil)) shouldBe "Nil"
    }

    test("pretty any-print null") {
        layout(any(null)) shouldBe "null"
    }

    test("pretty any-print None") {
        layout(any(None)) shouldBe "None"
    }

    test("pretty any-print Some") {
        layout(any(Some(1))) shouldBe "Some(1)"
    }

    test("pretty-print identifier") {
        layout("hello") shouldBe "hello"
    }

    test("pretty any-print identifier") {
        layout(any("hello")) shouldBe "\"hello\""
    }

    test("pretty any-print integer") {
        layout(any(1234)) shouldBe "1234"
    }

    test("pretty-print angles") {
        layout(angles(forwslash)) shouldBe "</>"
    }

    test("pretty-print brackets") {
        layout(brackets(backslash)) shouldBe "[\\]"
    }

    test("pretty-print squotes") {
        layout(squotes(dot)) shouldBe "'.'"
    }

    test("pretty-print empty sep sequence") {
        layout(sep(Nil)) shouldBe ""
    }

    test("pretty-print non-empty sep sequence - non-wrap") {
        layout(sep(List(langle, colon, rangle))) shouldBe "< : >"
    }

    test("pretty-print non-empty sep sequence - wrap") {
        layout(group(sep(List(langle, colon, rangle))), 2) shouldBe "<\n:\n>"
    }

    test("pretty-print empty hsep sequence") {
        layout(hsep(Nil)) shouldBe ""
    }

    test("pretty-print non-empty hsep sequence - non-wrap") {
        layout(hsep(List(langle, colon, rangle))) shouldBe "< : >"
    }

    test("pretty-print non-empty hsep sequence - wrap") {
        layout(group(hsep(List(langle, colon, rangle))), 2) shouldBe "< : >"
    }

    test("pretty-print empty fillsep sequence") {
        layout(fillsep(Nil)) shouldBe ""
    }

    test("pretty-print non-empty fillsep sequence - non-wrap") {
        layout(fillsep(List(langle, colon, rangle, colon, rangle))) shouldBe "< : > : >"
    }

    test("pretty-print non-empty fillsep sequence - wrap") {
        layout(group(fillsep(List(langle, colon, rangle, colon, rangle))), 3) shouldBe "< :\n> :\n>"
    }

    test("pretty-print empty fillsep sequence with sep") {
        layout(fillsep(Nil, comma)) shouldBe ""
    }

    test("pretty-print non-empty fillsep sequence with sep - non-wrap") {
        layout(fillsep(List(langle, colon, rangle, colon, rangle), comma)) shouldBe "<, :, >, :, >"
    }

    test("pretty-print non-empty fillsep sequence with sep - wrap") {
        layout(group(fillsep(List(langle, colon, rangle, colon, rangle), comma)), 3) shouldBe "<, :,\n>, :,\n>"
    }

    test("pretty-print empty lsep sequence") {
        layout(lsep(Nil, comma)) shouldBe ""
    }

    test("pretty-print non-empty lsep sequence - non-wrap") {
        layout(group(lsep(List(squote, dot, squote), comma)), 3) shouldBe "\n',\n.,\n'"
    }

    test("pretty-print empty lsep2 sequence") {
        layout(lsep2(Nil, comma)) shouldBe ""
    }

    test("pretty-print non-empty lsep2 sequence - non-wrap") {
        layout(group(lsep2(List(squote, dot, squote), comma)), 3) shouldBe "'\n, .\n, '\n"
    }

    val l = List(langle, dot, equal, rangle)

    test("pretty-print non-empty lsep sequence - wrap") {
        layout(group(lsep(l, comma)), 3) shouldBe "\n<,\n.,\n=,\n>"
    }

    test("pretty-print empty cat sequence") {
        layout(cat(Nil)) shouldBe ""
    }

    test("pretty-print non-empty cat sequence - non-wrap") {
        layout(cat(l)) shouldBe "<.=>"
    }

    test("pretty-print non-empty cat sequence - wrap") {
        layout(group(cat(l)), 3) shouldBe "<\n.\n=\n>"
    }

    test("pretty-print empty hcat sequence") {
        layout(hcat(Nil)) shouldBe ""
    }

    test("pretty-print non-empty hcat sequence - non-wrap") {
        layout(hcat(l)) shouldBe "<.=>"
    }

    test("pretty-print non-empty hcat sequence - wrap") {
        layout(group(hcat(l)), 3) shouldBe "<.=>"
    }

    test("pretty-print empty vcat sequence") {
        layout(vcat(Nil)) shouldBe ""
    }

    test("pretty-print non-empty vcat sequence - non-wrap") {
        layout(vcat(l)) shouldBe "<\n.\n=\n>"
    }

    test("pretty-print non-empty vcat sequence - wrap") {
        layout(group(vcat(l)), 3) shouldBe "<\n.\n=\n>"
    }

    test("pretty-print empty fillcat sequence") {
        layout(fillcat(Nil)) shouldBe ""
    }

    val m = List(langle, dot, equal, dot, equal, dot, equal, rangle)

    test("pretty-print non-empty fillcat sequence - non-wrap") {
        layout(fillcat(m)) shouldBe "<.=.=.=>"
    }

    test("pretty-print non-empty fillcat sequence - wrap") {
        layout(fillcat(m), 3) shouldBe "<.=\n.=.\n=>"
    }

    test("pretty-print empty sterm sequence") {
        layout(sterm(Nil, colon)) shouldBe ""
    }

    test("pretty-print non-empty sterm sequence - non-wrap") {
        layout(sterm(l, colon)) shouldBe "<:.:=:>:"
    }

    test("pretty-print non-empty sterm sequence - wrap") {
        layout((sterm(l, colon)), 3) shouldBe "<:\n.:\n=:\n>:"
    }

    test("pretty-print hanging text") {
        val words = "the hang combinator indents these words !".split(' ').toVector
        val d = hang(fillsep(words.map(text)), 3)
        layout(d, 15) shouldBe "the hang combinator\n   indents these\n   words !"
    }

    test("pretty-print indented text") {
        val d = indent("hi" <+> ("nice" <@> "world"), 2)
        layout(d, 5) shouldBe "  hi nice\n  world"
    }

    test("pretty-print aligned text") {
        val d = "hi" <+> ("nice" <%> "world")
        layout(d) shouldBe "hi nice\n   world"
    }

    test("pretty-print padded text") {
        val d = padto(10, "hi nice" <@> "world")
        layout(d) shouldBe "hi nice\nworld     "
    }

    test("pretty-print padded text - with linebreak") {
        val d = padtobreak(4, "hi nice") <> padtobreak(10, "world")
        layout(d) shouldBe "hi nice\n    world     "
    }

    val l1 = List(1, 2, 3)
    val l2 = List('a', 'b')

    test("pretty-print lists of simple values - non-wrap") {
        layout(list(l1)) shouldBe "List(1, 2, 3)"
    }

    test("pretty-print simple value arguments - non-wrap") {
        layout(arguments(l2)) shouldBe "(a, b)"
    }

    test("pretty-print lists of simple values - wrap") {
        layout(list(l2), 3) shouldBe "List(\n    a,\n    b)"
    }

    test("pretty-print simple value arguments - wrap") {
        layout(arguments(l1), 3) shouldBe "(\n    1,\n    2,\n    3)"
    }

    test("pretty-print lists of simple values - wrap, non-default") {
        layout(list(l1, "Foo", (_ : Int) => plus, semi, lterm), 3) shouldBe "Foo(\n    +;\n    +;\n    +;)"
    }

    test("pretty-print simple value arguments  - wrap, non-default") {
        layout(arguments(l2, (_ : Char) => equal, dot, lsep2), 3) shouldBe "(=\n    . =\n    )"
    }

    test("pretty-print sequences of simple values - non-wrap") {
        layout(seq(l1)) shouldBe "Seq(1, 2, 3)"
    }

    test("pretty-print sequences of simple values - wrap") {
        layout(seq(l1), 3) shouldBe "Seq(\n    1,\n    2,\n    3)"
    }

    case class Val(i : Int)
    val l3 = List(Val(1), Val(2), Val(3))

    test("pretty-print lists of structured values - non-wrap") {
        layout(list(l3)) shouldBe "List(Val(1), Val(2), Val(3))"
    }

    test("pretty-print lists of structured values - wrap") {
        layout(list(l3), 3) shouldBe "List(\n    Val(1),\n    Val(2),\n    Val(3))"
    }

    test("pretty-print sequences of structured values - non-wrap") {
        layout(seq(l3)) shouldBe "Seq(Val(1), Val(2), Val(3))"
    }

    test("pretty-print sequences of structured values - wrap") {
        layout(seq(l3), 3) shouldBe "Seq(\n    Val(1),\n    Val(2),\n    Val(3))"
    }

    test("pretty any-print empty vector") {
        layout(any(Vector())) shouldBe "Vector()"
    }

    test("pretty any-print singleton vector") {
        layout(any(Vector(1))) shouldBe "Vector(1)"
    }

    test("pretty any-print multiple-element vector") {
        layout(any(Vector(1, 2, 3))) shouldBe "Vector(1, 2, 3)"
    }

    test("pretty any-print empty map") {
        layout(any(Map())) shouldBe "Map()"
    }

    test("pretty any-print singleton map") {
        layout(any(Map(1 -> "One"))) shouldBe "Map(1 -> \"One\")"
    }

    test("pretty any-print multiple-element map") {
        layout(any(Map(1 -> "One", 2 -> "Two", 3 -> "Three"))) shouldBe "Map(1 -> \"One\", 2 -> \"Two\", 3 -> \"Three\")"
    }

    // Position map

    test("pretty-printing a doc with no linked nodes yields an empty position map") {
        val d = indent("hi" <+> ("nice" <@> "world"), 2)
        pretty(d).links.size shouldBe 0
    }

}

/**
 * Tests of parenthesis optimised pretty-printer module. The examples,
 * particularly Oberon0, test most cases; we just pick up the remainder
 * for coverage here.
 */
class ParenPrettyPrinterTests extends KiamaTests with ParenPrettyPrinter {

    abstract class Exp extends PrettyExpression

    case class PostOp(exp : Exp, prio : Int) extends Exp with PrettyUnaryExpression {
        def priority = prio
        def fixity = Postfix
        def op = "++"
    }

    case class PreOp(exp : Exp, prio : Int) extends Exp with PrettyUnaryExpression {
        def priority = prio
        def fixity = Prefix
        def op = "--"
    }

    case class BinOp(left : Exp, right : Exp, prio : Int, fix : Side) extends Exp with PrettyBinaryExpression {
        def priority = prio
        def fixity = Infix(fix)
        def op = "*"
    }

    case class If(condExp : Exp, thenExp : Exp, elseExp : Exp, prio : Int, fix : Side) extends Exp with PrettyNaryExpression {
        def priority = prio
        def fixity = Infix(fix)
    }

    case class Fi(condExp : Exp, thenExp : Exp, elseExp : Exp, prio : Int, fix : Side) extends Exp with PrettyNaryExpression {
        def priority = prio
        def fixity = Infix(fix)
    }

    case class Leaf(i : Int) extends Exp

    def toDoc(e : Exp) : Doc =
        toParenDoc(e)

    override def toParenDoc(e : PrettyExpression) : Doc =
        e match {
            case Leaf(i) =>
                value(i)
            case ei @ If(c, th, el, _, _) =>
                "if" <+> toDoc(c) <+> "then" <+> toDoc(th) <+> "else" <+>
                    recursiveToDoc(ei, el, RightAssoc)
            case ef @ Fi(c, th, el, _, _) =>
                recursiveToDoc(ef, c, LeftAssoc) <+> "then" <+> toDoc(th) <+>
                    "else" <+> toDoc(el) <+> "fi"
            case _ =>
                super.toParenDoc(e)
        }

    def toLayout(e : Exp) : String =
        layout(toDoc(e))

    // Postfix and prefix operators

    test("pretty-printing a lower priority postop on the left of a left assoc infix doesn't use parens") {
        val e = BinOp(PostOp(Leaf(1), 4), Leaf(2), 3, LeftAssoc)
        toLayout(e) shouldBe "1++ * 2"
    }

    test("pretty-printing a higher priority postop on the left of an infix doesn't use parens") {
        val e = BinOp(PostOp(Leaf(1), 2), Leaf(2), 3, LeftAssoc)
        toLayout(e) shouldBe "1++ * 2"
    }

    test("pretty-printing a lower priority preop on the left of an infix uses parens") {
        val e = BinOp(PreOp(Leaf(1), 4), Leaf(2), 3, LeftAssoc)
        toLayout(e) shouldBe "(--1) * 2"
    }

    test("pretty-printing a higher priority preop on the left of an infix doesn't use parens") {
        val e = BinOp(PreOp(Leaf(1), 2), Leaf(2), 3, LeftAssoc)
        toLayout(e) shouldBe "--1 * 2"
    }

    test("pretty-printing a lower priority postop on the right of an infix uses parens") {
        val e = BinOp(Leaf(2), PostOp(Leaf(1), 4), 3, LeftAssoc)
        toLayout(e) shouldBe "2 * (1++)"
    }

    test("pretty-printing a higher priority postop on the right of an infix doesn't use parens") {
        val e = BinOp(Leaf(2), PostOp(Leaf(1), 2), 3, LeftAssoc)
        toLayout(e) shouldBe "2 * 1++"
    }

    test("pretty-printing a lower priority preop on the right of an infix doesn't use parens") {
        val e = BinOp(Leaf(2), PreOp(Leaf(1), 4), 3, LeftAssoc)
        toLayout(e) shouldBe "2 * --1"
    }

    test("pretty-printing a higher priority preop on the right of an infix doesn't use parens") {
        val e = BinOp(Leaf(2), PreOp(Leaf(1), 2), 3, LeftAssoc)
        toLayout(e) shouldBe "2 * --1"
    }

    test("pretty-printing a lower priority postop on the left of a right assoc infix doesn't use parens") {
        val e = BinOp(PostOp(Leaf(1), 4), Leaf(2), 3, RightAssoc)
        toLayout(e) shouldBe "1++ * 2"
    }

    test("pretty-printing a higher priority postop on the left of a right assoc infix doesn't use parens") {
        val e = BinOp(PostOp(Leaf(1), 2), Leaf(2), 3, RightAssoc)
        toLayout(e) shouldBe "1++ * 2"
    }

    test("pretty-printing a lower priority preop on the left of a right assoc infix uses parens") {
        val e = BinOp(PreOp(Leaf(1), 4), Leaf(2), 3, RightAssoc)
        toLayout(e) shouldBe "(--1) * 2"
    }

    test("pretty-printing a higher priority preop on the left of a right assoc infix doesn't use parens") {
        val e = BinOp(PreOp(Leaf(1), 2), Leaf(2), 3, RightAssoc)
        toLayout(e) shouldBe "--1 * 2"
    }

    test("pretty-printing a lower priority postop on the right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(2), PostOp(Leaf(1), 4), 3, RightAssoc)
        toLayout(e) shouldBe "2 * (1++)"
    }

    test("pretty-printing a higher priority postop on the right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(2), PostOp(Leaf(1), 2), 3, RightAssoc)
        toLayout(e) shouldBe "2 * 1++"
    }

    test("pretty-printing a lower priority preop on the right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(2), PreOp(Leaf(1), 4), 3, RightAssoc)
        toLayout(e) shouldBe "2 * --1"
    }

    test("pretty-printing a higher priority preop on the right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(2), PreOp(Leaf(1), 2), 3, RightAssoc)
        toLayout(e) shouldBe "2 * --1"
    }

    // Right associative infix operator on right of other infix operators

    test("pretty-printing a lower-priority right assoc infix on the right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 2, RightAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 3, RightAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    test("pretty-printing a higher-priority right assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 4, RightAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    test("pretty-printing a lower-priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 2, LeftAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority right assoc infix on the right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 3, LeftAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority right assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 4, LeftAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    test("pretty-printing a lower-priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 2, NonAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority right assoc infix on the right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 3, NonAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority right assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, RightAssoc), 4, NonAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    // Left associative infix operator on right of other infix operators

    test("pretty-printing a lower-priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 2, RightAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority left assoc infix on the right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 3, RightAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority left assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 4, RightAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    test("pretty-printing a lower-priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 2, LeftAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority left assoc infix on the right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 3, LeftAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority left assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 4, LeftAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    test("pretty-printing a lower-priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 2, NonAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority left assoc infix on the right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 3, NonAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority left assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), 4, NonAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    // Non associative infix operator on right of other infix operators

    test("pretty-printing a lower-priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 2, RightAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority non assoc infix on the right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 3, RightAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority non assoc infix on the right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 4, RightAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    test("pretty-printing a lower-priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 2, LeftAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority non assoc infix on the right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 3, LeftAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority non assoc infix on the right of a left assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 4, LeftAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    test("pretty-printing a lower-priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 2, NonAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing an equal priority non assoc infix on the right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 3, NonAssoc)
        toLayout(e) shouldBe "1 * (2 * 3)"
    }

    test("pretty-printing a higher-priority non assoc infix on the right of a non assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), BinOp(Leaf(2), Leaf(3), 3, NonAssoc), 4, NonAssoc)
        toLayout(e) shouldBe "1 * 2 * 3"
    }

    // Right associative infix operator on left of other infix operators

    test("pretty-printing a lower-priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 2, RightAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority right assoc infix on the left of a right assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 3, RightAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority right assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 4, RightAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    test("pretty-printing a lower-priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 2, LeftAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority right assoc infix on the left of a left assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 3, LeftAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority right assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 4, LeftAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    test("pretty-printing a lower-priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 2, NonAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority right assoc infix on the left of a non assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 3, NonAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority right assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, RightAssoc), Leaf(1), 4, NonAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    // Left associative infix operator on left of other infix operators

    test("pretty-printing a lower-priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 2, RightAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority left assoc infix on the left of a right assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 3, RightAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority left assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 4, RightAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    test("pretty-printing a lower-priority left assoc infix on the left of a left assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 2, LeftAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 3, LeftAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    test("pretty-printing a higher-priority left assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 4, LeftAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    test("pretty-printing a lower-priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 2, NonAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority left assoc infix on the left of a non assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 3, NonAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority left assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(1), 4, NonAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    // Non associative infix operator on left of other infix operators

    test("pretty-printing a lower-priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 2, RightAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority non assoc infix on the left of a right assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 3, RightAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority non assoc infix on the left of a right assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 4, RightAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    test("pretty-printing a lower-priority non assoc infix on the left of a left assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 2, LeftAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority non assoc infix on the left of a left assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 3, LeftAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority non assoc infix on the left of a left assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 4, LeftAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    test("pretty-printing a lower-priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 2, NonAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing an equal priority non assoc infix on the left of a non assoc infix uses parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 3, NonAssoc)
        toLayout(e) shouldBe "(2 * 3) * 1"
    }

    test("pretty-printing a higher-priority non assoc infix on the left of a non assoc infix doesn't use parens") {
        val e = BinOp(BinOp(Leaf(2), Leaf(3), 3, NonAssoc), Leaf(1), 4, NonAssoc)
        toLayout(e) shouldBe "2 * 3 * 1"
    }

    // Right associative nary operators on the left of other operators

    test("pretty-printing a lower-priority right assoc nary infix on left of a right assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 2, RightAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority right assoc nary infix on left of a right assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 3, RightAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority right assoc nary infix on left of a right assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 4, RightAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    test("pretty-printing a lower-priority right assoc nary infix on left of a left assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 2, LeftAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority right assoc nary infix on left of a left assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 3, LeftAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority right assoc nary infix on left of a left assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 4, LeftAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    test("pretty-printing a lower-priority right assoc nary infix on left of a non assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 2, NonAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority right assoc nary infix on left of a non assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 3, NonAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority right assoc nary infix on left of a non assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, RightAssoc), Leaf(4), 4, NonAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    // Right associative nary operators on the right of other operators

    test("pretty-printing a lower-priority right assoc nary infix on right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 2, RightAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority right assoc nary infix on right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 3, RightAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    test("pretty-printing a higher-priority right assoc nary infix on right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 4, RightAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    test("pretty-printing a lower-priority right assoc nary infix on right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 2, LeftAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority right assoc nary infix on right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 3, LeftAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority right assoc nary infix on right of a left assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 4, LeftAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    test("pretty-printing a lower-priority right assoc nary infix on right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 2, NonAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority right assoc nary infix on right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 3, NonAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority right assoc nary infix on right of a non assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, RightAssoc), 4, NonAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    // Left associative nary operators on the left of other operators

    test("pretty-printing a lower-priority left assoc nary infix on left of a right assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 2, RightAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority left assoc nary infix on left of a right assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 3, RightAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority left assoc nary infix on left of a right assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 4, RightAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    test("pretty-printing a lower-priority left assoc nary infix on left of a left assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 2, LeftAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority left assoc nary infix on left of a left assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 3, LeftAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    test("pretty-printing a higher-priority left assoc nary infix on left of a left assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 4, LeftAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    test("pretty-printing a lower-priority left assoc nary infix on left of a non assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 2, NonAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority left assoc nary infix on left of a non assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 3, NonAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority left assoc nary infix on left of a non assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, LeftAssoc), Leaf(4), 4, NonAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    // Left associative nary operators on the right of other operators

    test("pretty-printing a lower-priority left assoc nary infix on right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 2, RightAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority left assoc nary infix on right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 3, RightAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority left assoc nary infix on right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 4, RightAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    test("pretty-printing a lower-priority left assoc nary infix on right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 2, LeftAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority left assoc nary infix on right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 3, LeftAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority left assoc nary infix on right of a left assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 4, LeftAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    test("pretty-printing a lower-priority left assoc nary infix on right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 2, NonAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority left assoc nary infix on right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 3, NonAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority left assoc nary infix on right of a non assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, LeftAssoc), 4, NonAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    // Non associative nary operators on the left of other operators

    test("pretty-printing a lower-priority non assoc nary infix on left of a right assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 2, RightAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority non assoc nary infix on left of a right assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 3, RightAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority non assoc nary infix on left of a right assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 4, RightAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    test("pretty-printing a lower-priority non assoc nary infix on left of a left assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 2, LeftAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority non assoc nary infix on left of a left assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 3, LeftAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority non assoc nary infix on left of a left assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 4, LeftAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    test("pretty-printing a lower-priority non assoc nary infix on left of a non assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 2, NonAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing an equal priority non assoc nary infix on left of a non assoc infix uses parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 3, NonAssoc)
        toLayout(e) shouldBe "(if 1 then 2 else 3) * 4"
    }

    test("pretty-printing a higher-priority non assoc nary infix on left of a non assoc infix doesn't use parens") {
        val e = BinOp(If(Leaf(1), Leaf(2), Leaf(3), 3, NonAssoc), Leaf(4), 4, NonAssoc)
        toLayout(e) shouldBe "if 1 then 2 else 3 * 4"
    }

    // Non associative nary operators on the right of other operators

    test("pretty-printing a lower-priority non assoc nary infix on right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 2, RightAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority non assoc nary infix on right of a right assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 3, RightAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority non assoc nary infix on right of a right assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 4, RightAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    test("pretty-printing a lower-priority non assoc nary infix on right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 2, LeftAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority non assoc nary infix on right of a left assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 3, LeftAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority non assoc nary infix on right of a left assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 4, LeftAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    test("pretty-printing a lower-priority non assoc nary infix on right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 2, NonAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing an equal priority non assoc nary infix on right of a non assoc infix uses parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 3, NonAssoc)
        toLayout(e) shouldBe "1 * (if 2 then 3 else 4)"
    }

    test("pretty-printing a higher-priority non assoc nary infix on right of a non assoc infix doesn't use parens") {
        val e = BinOp(Leaf(1), If(Leaf(2), Leaf(3), Leaf(4), 3, NonAssoc), 4, NonAssoc)
        toLayout(e) shouldBe "1 * if 2 then 3 else 4"
    }

    // nary infix with pre and post-components

    test("pretty-printing a lower priority postop on the right of an nary infix uses parens") {
        val e = If(Leaf(2), Leaf(3), PostOp(Leaf(1), 4), 3, NonAssoc)
        toLayout(e) shouldBe "if 2 then 3 else (1++)"
    }

    test("pretty-printing a higher priority postop on the right of an nary infix doesn't use parens") {
        val e = If(Leaf(2), Leaf(3), PostOp(Leaf(1), 2), 3, NonAssoc)
        toLayout(e) shouldBe "if 2 then 3 else 1++"
    }

    test("pretty-printing a lower priority preop on the left of an nary infix uses parens") {
        val e = Fi(PreOp(Leaf(1), 4), Leaf(2), Leaf(3), 3, NonAssoc)
        toLayout(e) shouldBe "(--1) then 2 else 3 fi"
    }

    test("pretty-printing a higher priority postop on the left of an nary infix doesn't use parens") {
        val e = Fi(PreOp(Leaf(1), 2), Leaf(2), Leaf(3), 3, NonAssoc)
        toLayout(e) shouldBe "--1 then 2 else 3 fi"
    }

}
