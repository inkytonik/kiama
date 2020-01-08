/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * Tests of source utility routiines.
 */
class SourceTests extends KiamaTests {

    import java.io.File.separator
    import org.bitbucket.inkytonik.kiama.util.Filenames.{cwd, dropCurrentPath, dropPrefix}

    val currentPath = cwd()
    val currentPathFile = currentPath + separator + "foo.txt"
    val currentPathBase = "foo.txt"
    val notCurrentPathFileWithSep = separator + "x" + currentPathFile
    val notCurrentPathFileWithoutSep = "x" + currentPathFile

    test("dropCurrentPath drops current path correctly") {
        dropCurrentPath(currentPathFile) shouldBe currentPathBase
    }

    test("dropCurrentPath drops partial current path correctly") {
        dropCurrentPath(currentPathFile) shouldBe currentPathBase
    }

    test("dropCurrentPath doesn't drop non-current path (with sep)") {
        dropCurrentPath(notCurrentPathFileWithSep) shouldBe notCurrentPathFileWithSep
    }

    test("dropCurrentPath doesn't drop non-current path (without sep)") {
        dropCurrentPath(notCurrentPathFileWithoutSep) shouldBe notCurrentPathFileWithoutSep
    }

    def makePath(p : String*) = p mkString separator
    def makePrefixPath(p : String*) = separator + makePath(p : _*)

    val testPath = makePrefixPath("foo", "bar", "ble.txt")
    val prefixOfTestPath1 = makePrefixPath("foo")
    val prefixOfTestPath2 = makePrefixPath("foo", "bar")
    val otherPath = makePath("bob", "harry")

    test("dropPrefix copes with empty filename") {
        dropPrefix("", testPath) shouldBe ""
    }

    test("dropPrefix correctly drops nothing if prefix is empty") {
        dropPrefix(testPath, "") shouldBe testPath
    }

    test("dropPrefix correctly drops prefix that is there (1)") {
        dropPrefix(testPath, prefixOfTestPath1) shouldBe "bar/ble.txt"
    }

    test("dropPrefix correctly drops prefix that is there (2)") {
        dropPrefix(testPath, prefixOfTestPath2) shouldBe "ble.txt"
    }

    test("dropPrefix correctly drops prefix that is whole filename") {
        dropPrefix(testPath, testPath) shouldBe ""
    }

    test("dropPrefix correctly ignores path that isn't there") {
        dropPrefix(testPath, otherPath) shouldBe testPath
    }

    test("dropPrefix correctly drops nothing if there is a partial common prefix (1)") {
        dropPrefix(prefixOfTestPath1, testPath) shouldBe prefixOfTestPath1
    }

    test("dropPrefix correctly drops nothing if there is a partial common prefix (2)") {
        dropPrefix(prefixOfTestPath2, testPath) shouldBe prefixOfTestPath2
    }

    test("dropPrefix correctly drops nothing if there is a partial common prefix (3)") {
        dropPrefix(prefixOfTestPath1, prefixOfTestPath2) shouldBe prefixOfTestPath1
    }

}
