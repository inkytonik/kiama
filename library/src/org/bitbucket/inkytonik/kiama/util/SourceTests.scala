/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2016 Anthony M Sloane, Macquarie University.
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
package util

/**
 * Tests of source utility routiines.
 */
class SourceTests extends Tests {

    import Source.dropPrefix

    def makePath(p : String*) = p mkString java.io.File.separator

    test("dropPrefix copes with empty filename") {
        dropPrefix("", makePath("foo", "bar")) shouldBe ""
    }

    test("dropPrefix correctly drops nothing if prefix is empty") {
        dropPrefix(makePath("foo", "bar", "ble.txt"), "") shouldBe makePath("foo", "bar", "ble.txt")
    }

    test("dropPrefix correctly drops prefix that is there") {
        dropPrefix(makePath("foo", "bar", "ble.txt"), makePath("foo", "bar")) shouldBe "ble.txt"
    }

    test("dropPrefix correctly drops prefix that is whole filename") {
        dropPrefix(makePath("foo", "bar", "ble.txt"), makePath("foo", "bar", "ble.txt")) shouldBe ""
    }

    test("dropPrefix correctly ignores prefix that isn't there") {
        dropPrefix(makePath("foo", "bar", "ble.txt"), makePath("bob", "harry")) shouldBe makePath("foo", "bar", "ble.txt")
    }

    test("dropPrefix correctly deals with filename that is prefix of prefix") {
        dropPrefix(makePath("foo", "bar"), makePath("foo", "bar", "ble.txt")) shouldBe ""
    }

    test("dropPrefix correctly deals with empty filename") {
        dropPrefix("", makePath("bob", "harry")) shouldBe ""
    }

    test("dropPrefix correctly deals with empty prefix") {
        dropPrefix(makePath("foo", "bar", "ble.txt"), "") shouldBe makePath("foo", "bar", "ble.txt")
    }

}
