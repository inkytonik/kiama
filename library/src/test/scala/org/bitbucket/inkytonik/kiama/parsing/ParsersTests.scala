/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
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
package parsing

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Basic tests of parsers.  Many cases are already tested in the examples, so
 * we only test for coverage here.
 */
class ParsersTests extends ParseTests {

    case class Node(i : Int)
    case class Tup2(n1 : Node, n2 : Node)
    case class Tup3(n1 : Node, n2 : Node, n3 : Node)
    case class Tup4(n1 : Node, n2 : Node, n3 : Node, n4 : Node)
    case class Tup5(n1 : Node, n2 : Node, n3 : Node, n4 : Node, n5 : Node)
    case class Tup6(n1 : Node, n2 : Node, n3 : Node, n4 : Node, n5 : Node, n6 : Node)
    case class NodeStr(n : Node, s : String)

    val parsers = new Parsers(positions)
    import parsers._

    lazy val node =
        parsers.regex("[0-9]+".r) ^^ (s => Node(s.toInt))

    test("arity 2 case class contructors can be used as parser actions") {
        val p = node ~ node ^^ Tup2
        p("1 2") should parseTo(Tup2(Node(1), Node(2)))
    }

    test("arity 3 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ^^ Tup3
        p("1 2 3") should parseTo(Tup3(Node(1), Node(2), Node(3)))
    }

    test("arity 4 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ~ node ^^ Tup4
        p("1 2 3 4") should parseTo(Tup4(Node(1), Node(2), Node(3), Node(4)))
    }

    test("arity 5 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ~ node ~ node ^^ Tup5
        p("1 2 3 4 5") should parseTo(Tup5(Node(1), Node(2), Node(3), Node(4), Node(5)))
    }

    test("arity 6 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ~ node ~ node ~ node ^^ Tup6
        p("1 2 3 4 5 6") should parseTo(Tup6(Node(1), Node(2), Node(3), Node(4), Node(5), Node(6)))
    }

    test("arity 2 tuples can be created by parsers without explicit actions") {
        val p : Parser[(Node, Node)] =
            node ~ node
        p("1 2") should parseTo((Node(1), Node(2)))
    }

    test("arity 3 tuples can be created by parsers without explicit actions") {
        val p : Parser[(Node, Node, Node)] =
            node ~ node ~ node
        p("1 2 3") should parseTo((Node(1), Node(2), Node(3)))
    }

    test("arity 4 tuples can be created by parsers without explicit actions") {
        val p : Parser[(Node, Node, Node, Node)] =
            node ~ node ~ node ~ node
        p("1 2 3 4") should parseTo((Node(1), Node(2), Node(3), Node(4)))
    }

    test("arity 5 tuples can be created by parsers without explicit actions") {
        val p : Parser[(Node, Node, Node, Node, Node)] =
            node ~ node ~ node ~ node ~ node
        p("1 2 3 4 5") should parseTo((Node(1), Node(2), Node(3), Node(4), Node(5)))
    }

    test("arity 6 tuples can be created by parsers without explicit actions") {
        val p : Parser[(Node, Node, Node, Node, Node, Node)] =
            node ~ node ~ node ~ node ~ node ~ node
        p("1 2 3 4 5 6") should parseTo((Node(1), Node(2), Node(3), Node(4), Node(5), Node(6)))
    }

    {
        val alphaNode =
            parsers.regex("a[0-9]+".r) ^^ (s => Node(s.tail.toInt))

        val p1a = node ~ "foo"
        val p1b = node ~/ "foo"
        val p1bl = node <~/ "foo"
        val p1br = node ~/> "foo"

        val p2 = node ~ "bar"
        val p2l = node <~ "bar"
        val p2r = node ~> "bar"

        val p3 = alphaNode ~ "bar"
        val p3l = alphaNode <~ "bar"
        val p3r = alphaNode ~> "bar"

        test("normal sequence interacts properly with alternation") {
            val p = p1a | p2
            p("1 foo") should parseTo(new ~(Node(1), "foo"))
            p("1 bar") should parseTo(new ~(Node(1), "bar"))
            p("a2 bar") should failParseAt(1, 1, "string matching regex '[0-9]+' expected but 'a' found")
        }

        test("non-backtracking sequence interacts properly with alternation") {
            val p = p1b | p2 | p3
            p("1 foo") should parseTo(new ~(Node(1), "foo"))
            p("1 bar") should errorParseAt(1, 3, "'foo' expected but 'b' found")
            p("a2 bar") should parseTo(new ~(Node(2), "bar"))

            val pl = p1bl | p2l | p3l
            pl("1 foo") should parseTo(Node(1))
            pl("1 bar") should errorParseAt(1, 3, "'foo' expected but 'b' found")
            pl("a2 bar") should parseTo(Node(2))

            val pr = p1br | p2r | p3r
            pr("1 foo") should parseTo("foo")
            pr("1 bar") should errorParseAt(1, 3, "'foo' expected but 'b' found")
            pr("a2 bar") should parseTo("bar")
        }
    }

    {
        // FastParse cut examples: http://www.lihaoyi.com/fastparse/#Cuts

        val alphas = parsers.regex("[a-z]+".r)

        test("non-backtracking sequence operator properly cuts") {
            val valp = "val" ~/ alphas
            val defp = "def" ~/ alphas
            val nocut = valp | defp
            nocut("val abcd") should parseTo(new ~("val", "abcd"))
            nocut("val 1234") should errorParseAt(1, 5, "string matching regex '[a-z]+' expected but '1' found")
        }

        test("cut sequence operator properly cuts inside repetition") {
            val stmt = "val" ~/ alphas <~ ";"
            val stmts = phrase(rep1(stmt))
            stmts("val abcd;") should parseTo(Vector(new ~("val", "abcd")))
            stmts("val abcd; val efg;") should parseTo(
                Vector(new ~("val", "abcd"), new ~("val", "efg"))
            )
            stmts("val abcd; val") should errorParseAt(1, 14, "string matching regex '[a-z]+' expected but end of source found")
        }

        val digit = parsers.regex("[0-9]".r)
        val time1 = (opt("1") ~ digit ~ ":" ~/ (digit ~ digit ~ ("am" | "pm"))) ^^^ (())
        val time2 = (opt("1" | "2") ~ digit ~ ":" ~/ (digit ~ digit)) ^^^ (())

        test("cut-based time parsers work in isolation") {
            time1("12:30pm") should parseTo(())
            time2("17:45") should parseTo(())
        }

        test("cut nested in time parsers propagate outwards") {
            val time = time1 | time2
            time("12:30pm") should parseTo(())
            time("17:45") should errorParseAt(1, 6, "'pm' expected but end of source found")
        }

        test("nocut successfully supresses nested cut in time parsers") {
            val time = nocut(time1) | time2
            time("12:30pm") should parseTo(())
            time("17:45") should parseTo(())
        }
    }

    test("error parser combinator skips whitespace and gives correct error") {
        val p = error("MESSAGE")
        p("foo") should errorParseAt(1, 1, "MESSAGE")
        p("   foo") should errorParseAt(1, 4, "MESSAGE")
        p("  \n  foo") should errorParseAt(2, 3, "MESSAGE")
    }

    test("failure parser combinator skips whitespace and gives correct failure") {
        val p = failure("MESSAGE")
        p("foo") should failParseAt(1, 1, "MESSAGE")
        p("   foo") should failParseAt(1, 4, "MESSAGE")
        p("  \n  foo") should failParseAt(2, 3, "MESSAGE")
    }

    {
        val p = keywords("[^a-z]".r, List("one", "two"))

        test("keywords parser works if whitespace is after the keyword") {
            p("one ") should parseTo("one ")
        }

        test("keywords parser works if EOI is after the keyword") {
            p("two") should parseTo("two")
        }

        test("keywords parser fails if keyword is just a prefix of input") {
            p("ones") should failParseAt(1, 1, """string matching regex '(one|two)([^a-z]|\z)' expected but 'o' found""")
        }
    }

}
