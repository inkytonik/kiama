/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package rewriting

/**
 * Types that implement this interface can be rewritten using the methods
 * of Kiama's Rewriter library.  Implementing this interface is not
 * necessary if the type implements Product (which automatically includes
 * all case class instances and case objects).
 */
trait Rewritable {

    import scala.collection.immutable.Seq

    /**
     * Return the number of components that this value has.  Should be
     * greater than or equal to zero.
     */
    def arity : Int

    /**
     * Return a finite sequence containing the components of this value.
     */
    def deconstruct : Seq[Any]

    /**
     * Return a new value constructed from the given original value and
     * the given a finite sequence of the new components. In most cases,
     * the new value should be of the same type as the original with the
     * new components, but this is not required.  This method should throw an
     * IllegalArgumentException if any of the components are not
     * appropriate (e.g., there is the wrong number of them or they are
     * of the wrong type).
     */
    def reconstruct(components : Seq[Any]) : Any

    /**
     * Helper function that can be called by implementations of reconstruct
     * to report an error.  desc is a description of the structure that
     * was being constructed, argtypes is a string describing the expected
     * types of the arguments and args is the argument sequence that was
     * provided.  An IllegalArgumentException containing a description
     * of the problem is thrown.
     */
    protected def illegalArgs(desc : String, argtypes : String, args : Seq[Any]) =
        throw (new IllegalArgumentException(s"making $desc: expecting $argtypes, got ${args.mkString(", ")}"))

}
