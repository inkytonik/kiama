/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
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
package example.json

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
class SyntaxAnalyserTests extends ParseTests {

    import JSONTree._

    val parsers = new SyntaxAnalyser(positions)
    import parsers._

    test("parsing true works") {
        jtrue("true") should parseTo(JTrue())
    }

    test("parsing false works") {
        jfalse("false") should parseTo(JFalse())
    }

    test("parsing null works") {
        jnull("null") should parseTo(JNull())
    }

    // Number tests

    test("parsing a single digit integer works (0)") {
        jnumber("0") should parseTo(JNumber(0.0))
    }

    test("parsing a single digit integer works (4)") {
        jnumber("4") should parseTo(JNumber(4.0))
    }

    test("parsing a single digit integer works (-7)") {
        jnumber("-7") should parseTo(JNumber(-7.0))
    }

    test("parsing a single digit integer works (-9)") {
        jnumber("-9") should parseTo(JNumber(-9.0))
    }

    test("parsing a non-trivial integer works (78)") {
        jnumber("78") should parseTo(JNumber(78.0))
    }

    test("parsing a non-trivial integer works (-123)") {
        jnumber("-123") should parseTo(JNumber(-123.0))
    }

    test("parsing a non-trivial integer works (793223)") {
        jnumber("793223") should parseTo(JNumber(793223.0))
    }

    test("parsing non-numbers as numbers fails (x)") {
        jnumber("x") should failParseAt(1, 1,
            """string matching regex '-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?' expected but 'x' found""")
    }

    test("parsing non-numbers as numbers fails (Eugene)") {
        jnumber("Eugene") should failParseAt(1, 1,
            """string matching regex '-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?' expected but 'E' found""")
    }

    test("parsing non-numbers as numbers fails (paren)") {
        jnumber("(") should failParseAt(1, 1,
            """string matching regex '-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?' expected but '(' found""")
    }

    test("parsing a floating point number without exponent works (0.9)") {
        jnumber("0.9") should parseTo(JNumber(0.9))
    }

    test("parsing a floating point number without exponent works (-1.4)") {
        jnumber("-1.4") should parseTo(JNumber(-1.4))
    }

    test("parsing a floating point number without exponent works (42.09)") {
        jnumber("42.09") should parseTo(JNumber(42.09))
    }

    test("parsing a floating point number without exponent works (1234.5678)") {
        jnumber("1234.5678") should parseTo(JNumber(1234.5678))
    }

    test("parsing a floating point number without decimal part but with exponent works (0e5)") {
        jnumber("0e5") should parseTo(JNumber(0.0))
    }

    test("parsing a floating point number without decimal part but with exponent works (-4e+3)") {
        jnumber("-4e+3") should parseTo(JNumber(-4e3))
    }

    test("parsing a floating point number without decimal part but with exponent works (73e-6)") {
        jnumber("73e-6") should parseTo(JNumber(73e-6))
    }

    test("parsing a floating point number with decimal part and exponent works (8.5e2)") {
        jnumber("8.5e2") should parseTo(JNumber(8.5e2))
    }

    test("parsing a floating point number with decimal part and exponent works (-0.7e-5)") {
        jnumber("-0.7e-5") should parseTo(JNumber(-0.7e-5))
    }

    test("parsing a floating point number with decimal part and exponent works (96.0001e+13)") {
        jnumber("96.0001e+13") should parseTo(JNumber(96.0001e13))
    }

    test("parsing a number with leading zeros doesn't work (00)") {
        jnumber("01") should parseTo(JNumber(0))
    }

    test("parsing a number with leading zeros doesn't work (000)") {
        jnumber("011") should parseTo(JNumber(0))
    }

    test("parsing a number with leading zeros doesn't work (01)") {
        jnumber("01") should parseTo(JNumber(0))
    }

    test("parsing a number with leading zeros doesn't work (000.1)") {
        jnumber("000.1") should parseTo(JNumber(0))
    }

    // String

    test("empty string parses") {
        jstring("\"\"") should parseTo(JString(""))
    }

    test("simple string parses") {
        jstring("\"hello there\"") should parseTo(JString("hello there"))
    }

    // The Unicode escape in the next test is split into two strings so that
    // Scala doesn't convert it.  Surprisingly, it will do this in """..."""
    // strings, even though it doesn't for other escapes.

    // FIXME
    // test ("string with escapes parses") {
    //     jstring(""""\\\/\b\f\n\r\t\""" + """uAB38"""") should parseTo(
    //         JString ("\"\\/\b\f\n\r\t\uAB38\"")
    //     )
    // }

    // Array

    test("empty array parses") {
        jarray("[]") should parseTo(JArray(Vector()))
    }

    test("single element array parses") {
        jarray("[ 1 ]") should parseTo(JArray(Vector(JNumber(1))))
    }

    test("multiple element array parses") {
        jarray("[ \"a\", null, true ]") should parseTo(
            JArray(Vector(JString("a"), JNull(), JTrue()))
        )
    }

    test("nested array parses") {
        jarray("[ false, [ 10, 20 ], 30 ]") should parseTo(
            JArray(Vector(
                JFalse(),
                JArray(Vector(JNumber(10), JNumber(20))),
                JNumber(30)
            ))
        )
    }

    test("object inside array parses") {
        jarray("[ false, { \"f\" : 10, \"g\" : 20 }, 30 ]") should parseTo(
            JArray(Vector(
                JFalse(),
                JObject(Vector(
                    JName("f") -> JNumber(10),
                    JName("g") -> JNumber(20)
                )),
                JNumber(30)
            ))
        )
    }

    // Object

    test("empty object parses") {
        jobject("{}") should parseTo(JObject(Vector()))
    }

    test("single pair object parses") {
        jobject("{ \"one\" : 1 }") should parseTo(
            JObject(Vector(JName("one") -> JNumber(1)))
        )
    }

    test("multiple pair object parses") {
        jobject("{ \"a\" : \"a\", \"b\" : null, \"c\" : true }") should parseTo(
            JObject(Vector(
                JName("a") -> JString("a"),
                JName("b") -> JNull(),
                JName("c") -> JTrue()
            ))
        )
    }

    test("nested object parses") {
        jobject("{ \"e\" : false, \"f\" : { \"g\" : 10, \"h\" : 20 }, \"i\" : 30 }") should parseTo(
            JObject(Vector(
                JName("e") -> JFalse(),
                JName("f") -> JObject(Vector(
                    JName("g") -> JNumber(10),
                    JName("h") -> JNumber(20)
                )),
                JName("i") -> JNumber(30)
            ))
        )
    }

    test("array inside object parses") {
        jobject("{ \"e\" : false, \"f\" : [ 10, 20 ], \"i\" : 30 }") should parseTo(
            JObject(Vector(
                JName("e") -> JFalse(),
                JName("f") -> JArray(Vector(JNumber(10), JNumber(20))),
                JName("i") -> JNumber(30)
            ))
        )
    }

}
