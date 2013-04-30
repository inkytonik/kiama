/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2013 Anthony M Sloane, Macquarie University.
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
package example.transform

import AST.Program
import org.kiama.util.Compiler

/**
 * Main program for transformation compiler.
 */
class Driver extends Parser with Compiler[Program] {

    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    override def process (filename : String, program : Program, console : Console, emitter : Emitter) : Boolean = {

        import Analysis._

        super.process (filename, program, console, emitter)

        // Print original program and obtain "no priority" expression
        emitter.emitln (program)
        val expr = program.expr

        // Check for semantic errors on the original expression.  This
        // will cause a translation to a priority-correct representation
        // and error computation on that rep.
        resetmessages
        expr->errors

        // For testing, print the priority-correct representation
        emitter.emitln (expr->ast)

        // Report any semantic errors
        if (messagecount > 0) {
            report (emitter)
            false
        } else
            true

    }

}

/**
 * Transformation compiler main program.
 */
object Main extends Driver
