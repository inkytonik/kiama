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

    import org.bitbucket.inkytonik.kiama.util.Positions

    case class Node(i : Int)
    case class Tup2(n1 : Node, n2 : Node)
    case class Tup3(n1 : Node, n2 : Node, n3 : Node)
    case class Tup4(n1 : Node, n2 : Node, n3 : Node, n4 : Node)
    case class Tup5(n1 : Node, n2 : Node, n3 : Node, n4 : Node, n5 : Node)
    case class Tup6(n1 : Node, n2 : Node, n3 : Node, n4 : Node, n5 : Node, n6 : Node)

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
