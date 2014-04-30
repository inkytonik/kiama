/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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
 * Compatibility interface so variances between Scala 2.10 and 2.11 can be
 * accommodated in the same client code. This class is deprecated so that
 * warnings about 2.10 things that are deprecated in 2.11 are hidden. Do
 * not use this class directly, but import from its companion object so
 * that the deprecation warnings will be suppressed.
 */
@deprecated ("You should never see this message", "2.11")
class Compat210 {

    /**
     * Make 2.10 macro contexts available under a "blackbox" prefix.
     * See https://issues.scala-lang.org/browse/SI-8209.
     */
    object blackbox {
        type Context = scala.reflect.macros.Context
    }

    /**
     * Provide a link from io.StdIn.readLine to the 2.10 version on
     * the Console object.
     */
    object io {
        object StdIn {
            def readLine (text : String, args : Any*) : String =
                Console.readLine (text, args)
        }
    }

}

/**
 * Object by which to access the compatibility interface. We don't just put
 * the compatibility definitions in this object because using a class and
 * companion object suppresses deprecation warnings.
 */
object Compat210 extends Compat210
