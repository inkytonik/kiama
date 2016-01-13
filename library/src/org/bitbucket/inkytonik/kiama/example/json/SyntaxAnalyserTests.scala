/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2016 Anthony M Sloane, Macquarie University.
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
        assertParseOk("true", jtrue, JTrue())
    }

    test("parsing false works") {
        assertParseOk("false", jfalse, JFalse())
    }

    test("parsing null works") {
        assertParseOk("null", jnull, JNull())
    }

    // Number tests

    test("parsing a single digit integer works (0)") {
        assertParseOk("0", jnumber, JNumber(0.0))
    }

    test("parsing a single digit integer works (4)") {
        assertParseOk("4", jnumber, JNumber(4.0))
    }

    test("parsing a single digit integer works (-7)") {
        assertParseOk("-7", jnumber, JNumber(-7.0))
    }

    test("parsing a single digit integer works (-9)") {
        assertParseOk("-9", jnumber, JNumber(-9.0))
    }

    test("parsing a non-trivial integer works (78)") {
        assertParseOk("78", jnumber, JNumber(78.0))
    }

    test("parsing a non-trivial integer works (-123)") {
        assertParseOk("-123", jnumber, JNumber(-123.0))
    }

    test("parsing a non-trivial integer works (793223)") {
        assertParseOk("793223", jnumber, JNumber(793223.0))
    }

    test("parsing non-numbers as numbers fails (x)") {
        assertParseError("x", jnumber, 1, 1,
            """string matching regex `-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?' expected but `x' found""")
    }

    test("parsing non-numbers as numbers fails (Eugene)") {
        assertParseError("Eugene", jnumber, 1, 1,
            """string matching regex `-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?' expected but `E' found""")
    }

    test("parsing non-numbers as numbers fails (paren)") {
        assertParseError("(", jnumber, 1, 1,
            """string matching regex `-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?' expected but `(' found""")
    }

    test("parsing a floating point number without exponent works (0.9)") {
        assertParseOk("0.9", jnumber, JNumber(0.9))
    }

    test("parsing a floating point number without exponent works (-1.4)") {
        assertParseOk("-1.4", jnumber, JNumber(-1.4))
    }

    test("parsing a floating point number without exponent works (42.09)") {
        assertParseOk("42.09", jnumber, JNumber(42.09))
    }

    test("parsing a floating point number without exponent works (1234.5678)") {
        assertParseOk("1234.5678", jnumber, JNumber(1234.5678))
    }

    test("parsing a floating point number without decimal part but with exponent works (0e5)") {
        assertParseOk("0e5", jnumber, JNumber(0.0))
    }

    test("parsing a floating point number without decimal part but with exponent works (-4e+3)") {
        assertParseOk("-4e+3", jnumber, JNumber(-4e3))
    }

    test("parsing a floating point number without decimal part but with exponent works (73e-6)") {
        assertParseOk("73e-6", jnumber, JNumber(73e-6))
    }

    test("parsing a floating point number with decimal part and exponent works (8.5e2)") {
        assertParseOk("8.5e2", jnumber, JNumber(8.5e2))
    }

    test("parsing a floating point number with decimal part and exponent works (-0.7e-5)") {
        assertParseOk("-0.7e-5", jnumber, JNumber(-0.7e-5))
    }

    test("parsing a floating point number with decimal part and exponent works (96.0001e+13)") {
        assertParseOk("96.0001e+13", jnumber, JNumber(96.0001e13))
    }

    test("parsing a number with leading zeros doesn't work (00)") {
        assertParseError("00", jnumber, 1, 2, "end of input expected")
    }

    test("parsing a number with leading zeros doesn't work (000)") {
        assertParseError("000", jnumber, 1, 2, "end of input expected")
    }

    test("parsing a number with leading zeros doesn't work (01)") {
        assertParseError("01", jnumber, 1, 2, "end of input expected")
    }

    test("parsing a number with leading zeros doesn't work (000.1)") {
        assertParseError("000.1", jnumber, 1, 2, "end of input expected")
    }

    // String

    test("empty string parses") {
        assertParseOk("\"\"", jstring,
            JString(""))
    }

    test("simple string parses") {
        assertParseOk("\"hello there\"", jstring,
            JString("hello there"))
    }

    // The Unicode escape in the next test is split into two strings so that
    // Scala doesn't convert it.  Surprisingly, it will do this in """..."""
    // strings, even though it doesn't for other escapes.

    // FIXME
    // test ("string with escapes parses") {
    //     assertParseOk (""""\\\/\b\f\n\r\t\""" + """uAB38"""", jstring,
    //         JString ("\"\\/\b\f\n\r\t\uAB38\""))
    // }

    // Array

    test("empty array parses") {
        assertParseOk("[]", jarray,
            JArray(Vector()))
    }

    test("single element array parses") {
        assertParseOk("[ 1 ]", jarray,
            JArray(Vector(JNumber(1))))
    }

    test("multiple element array parses") {
        assertParseOk("[ \"a\", null, true ]", jarray,
            JArray(Vector(JString("a"), JNull(), JTrue())))
    }

    test("nested array parses") {
        assertParseOk("[ false, [ 10, 20 ], 30 ]", jarray,
            JArray(Vector(
                JFalse(),
                JArray(Vector(JNumber(10), JNumber(20))),
                JNumber(30)
            )))
    }

    test("object inside array parses") {
        assertParseOk("[ false, { \"f\" : 10, \"g\" : 20 }, 30 ]", jarray,
            JArray(Vector(
                JFalse(),
                JObject(Vector(
                    JName("f") -> JNumber(10),
                    JName("g") -> JNumber(20)
                )),
                JNumber(30)
            )))
    }

    // Object

    test("empty object parses") {
        assertParseOk("{}", jobject,
            JObject(Vector()))
    }

    test("single pair object parses") {
        assertParseOk("{ \"one\" : 1 }", jobject,
            JObject(Vector(JName("one") -> JNumber(1))))
    }

    test("multiple pair object parses") {
        assertParseOk("{ \"a\" : \"a\", \"b\" : null, \"c\" : true }", jobject,
            JObject(Vector(
                JName("a") -> JString("a"),
                JName("b") -> JNull(),
                JName("c") -> JTrue()
            )))
    }

    test("nested object parses") {
        assertParseOk("{ \"e\" : false, \"f\" : { \"g\" : 10, \"h\" : 20 }, \"i\" : 30 }", jobject,
            JObject(Vector(
                JName("e") -> JFalse(),
                JName("f") -> JObject(Vector(
                    JName("g") -> JNumber(10),
                    JName("h") -> JNumber(20)
                )),
                JName("i") -> JNumber(30)
            )))
    }

    test("array inside object parses") {
        assertParseOk("{ \"e\" : false, \"f\" : [ 10, 20 ], \"i\" : 30 }", jobject,
            JObject(Vector(
                JName("e") -> JFalse(),
                JName("f") -> JArray(Vector(JNumber(10), JNumber(20))),
                JName("i") -> JNumber(30)
            )))
    }

}
