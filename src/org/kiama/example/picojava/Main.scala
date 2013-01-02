/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package example.picojava

import AbstractSyntax.Program
import org.kiama.util.Compiler

object Main extends Compiler[Program] with Parser {

    import ErrorCheck.errors
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    override def process (program : Program, console : Console, emitter : Emitter) : Boolean = {

        super.process (program, console, emitter)

        resetmessages
        program->errors

        if (messagecount > 0) {
            report (emitter)
            false
        } else
            true

    }

}
