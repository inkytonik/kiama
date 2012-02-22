/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
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
 * Basic tests of parser utilities module.  Many cases are already tested
 * in the examples, so we only test for coverage here.
 */
@RunWith(classOf[JUnitRunner])
class ParserUtilitiesTests extends RegexParserTests with ParserUtilities {

    case class Node (i : Int)
    case class Tup2 (n1 : Node, n2 : Node)
    case class Tup3 (n1 : Node, n2 : Node, n3 : Node)
    case class Tup4 (n1 : Node, n2 : Node, n3 : Node, n4 : Node)
    case class Tup5 (n1 : Node, n2 : Node, n3 : Node, n4 : Node, n5 : Node)
    case class Tup6 (n1 : Node, n2 : Node, n3 : Node, n4 : Node, n5 : Node, n6 : Node)

    lazy val node = "[0-9]+".r ^^ (s => Node (s.toInt))

    test ("arity 2 case class contructors can be used as parser actions") {
        val p = node ~ node ^^ Tup2
        assertParseOk ("1 2", p, Tup2 (Node (1), Node (2)))
    }

    test ("arity 3 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ^^ Tup3
        assertParseOk ("1 2 3", p, Tup3 (Node (1), Node (2), Node (3)))
    }

    test ("arity 4 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ~ node ^^ Tup4
        assertParseOk ("1 2 3 4", p, Tup4 (Node (1), Node (2), Node (3), Node (4)))
    }

    test ("arity 5 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ~ node ~ node ^^ Tup5
        assertParseOk ("1 2 3 4 5", p, Tup5 (Node (1), Node (2), Node (3), Node (4), Node (5)))
    }

    test ("arity 6 case class contructors can be used as parser actions") {
        val p = node ~ node ~ node ~ node ~ node ~ node ^^ Tup6
        assertParseOk ("1 2 3 4 5 6", p, Tup6 (Node (1), Node (2), Node (3), Node (4), Node (5), Node (6)))
    }

    test ("arity 2 tuples can be created by parsers without explicit actions") {
        val p : PackratParser[(Node,Node)] =
            node ~ node
        assertParseOk ("1 2", p, (Node (1), Node (2)))
    }

    test ("arity 3 tuples can be created by parsers without explicit actions") {
        val p : PackratParser[(Node,Node,Node)] =
            node ~ node ~ node
        assertParseOk ("1 2 3", p, (Node (1), Node (2), Node (3)))
    }

    test ("arity 4 tuples can be created by parsers without explicit actions") {
        val p : PackratParser[(Node,Node,Node,Node)] =
            node ~ node ~ node ~ node
        assertParseOk ("1 2 3 4", p, (Node (1), Node (2), Node (3), Node (4)))
    }

    test ("arity 5 tuples can be created by parsers without explicit actions") {
        val p : PackratParser[(Node,Node,Node,Node,Node)] =
            node ~ node ~ node ~ node ~ node
        assertParseOk ("1 2 3 4 5", p, (Node (1), Node (2), Node (3), Node (4), Node (5)))
    }

    test ("arity 6 tuples can be created by parsers without explicit actions") {
        val p : PackratParser[(Node,Node,Node,Node,Node,Node)] =
            node ~ node ~ node ~ node ~ node ~ node
        assertParseOk ("1 2 3 4 5 6", p, (Node (1), Node (2), Node (3), Node (4), Node (5), Node (6)))
    }

    test ("failure parser combinator skips whitespace and gives correct failure") {
        val p = failure ("MESSAGE")
        assertParseError ("foo", p, 1, 1, "MESSAGE")
        assertParseError ("   foo", p, 1, 4, "MESSAGE")
        assertParseError ("  \n  foo", p, 2, 3, "MESSAGE")
    }

    test ("err parser combinator skips whitespace and gives correct error") {
        val p = err ("MESSAGE")
        assertParseError ("foo", p, 1, 1, "MESSAGE", true)
        assertParseError ("   foo", p, 1, 4, "MESSAGE", true)
        assertParseError ("  \n  foo", p, 2, 3, "MESSAGE", true)
    }

}
