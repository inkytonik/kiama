/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2015 Anthony M Sloane, Macquarie University.
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

package org

/**
 * Kiama is a Scala library for language processing. It is a project of the
 * Programming Languages Research Group at Macquarie University. For full
 * project details see the web site `http://kiama.googlecode.com`.
 *
 * Kiama's main components address tree decoration via attribute grammars
 * (package `attribution`), tree transformation via strategic term rewriting
 * (package `rewriting`), dynamic semantics (package `machine`) and
 * pretty-printing (package `output`).
 *
 * The `util` package contains support modules for parsing,  input/output,
 * read-eval-print loops (REPLs) and pattern matching.
 *
 * The `examples` package (available as part of the Kiama tests) contains
 * many examples of using Kiama to solve small to medium language processing
 * problems.
 */
package object kiama {

    /**
     * Convenient type constructor for partial functions.
     */
    type ==>[T,U] = PartialFunction[T,U]

}
