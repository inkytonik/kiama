/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015 Anthony M Sloane, Macquarie University.
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

/**
 * Tests of messaging utility routiines. Most tests of messaging
 * are in the examples.
 */
class MessagingTests extends Tests {

    import Messaging.dropPrefix

    test ("dropPrefix correctly drops prefix that is there") {
        assertResult ("ble.txt") (dropPrefix ("/foo/bar/ble.txt", "/foo/bar"))
    }

    test ("dropPrefix correctly drops prefix that is whole filename") {
        assertResult ("") (dropPrefix ("/foo/bar/ble.txt", "/foo/bar/ble.txt"))
    }

    test ("dropPrefix correctly ignores prefix that isn't there") {
        assertResult ("/foo/bar/ble.txt") (dropPrefix ("/foo/bar/ble.txt", "bob/harry"))
    }

    test ("dropPrefix correctly deals with filename that is prefix of prefix") {
        assertResult ("") (dropPrefix ("/foo/bar", "/foo/bar/ble.txt"))
    }

    test ("dropPrefix correctly deals with empty filename") {
        assertResult ("") (dropPrefix ("", "/bob/harry"))
    }

    test ("dropPrefix correctly deals with empty prefix") {
        assertResult ("/foo/bar/ble.txt") (dropPrefix ("/foo/bar/ble.txt", ""))
    }

// FIXME:
// empty path
// empty prefix
// path == prefix
// path shorter than prefix

}
