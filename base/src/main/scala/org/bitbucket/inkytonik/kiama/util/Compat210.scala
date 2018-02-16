/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * Compatibility interface so variances between Scala 2.10 and 2.11 can be
 * accommodated in the same client code. This class is deprecated so that
 * warnings about 2.10 things that are deprecated in 2.11 are hidden. Do
 * not use this class directly, but import from its companion object so
 * that the deprecation warnings will be suppressed.
 */
@deprecated("You should never see this message", "2.11")
class Compat210 {

    /**
     * Dummy value that can be referred to in importing context. Useful
     * for avoiding warnings about unused import on 2.11.
     */
    val dummy = 0

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
            def readLine(text : String, args : Any*) : String =
                Console.readLine(text, args)
        }
    }

}

/**
 * Object by which to access the compatibility interface. We don't just put
 * the compatibility definitions in this object because using a class and
 * companion object suppresses deprecation warnings.
 */
object Compat210 extends Compat210
