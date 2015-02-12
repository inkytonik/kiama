/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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
package example.til

import TILTree.Program
import org.kiama.util.{Compiler, Config}

/**
 * Main program for TIL chairmarks that just parse and print their ASTs
 * to standard output.
 */
trait ParsingMain extends Compiler[Program] {

    import org.kiama.util.Emitter

    def process (filename : String, ast : Program, config : Config) {
        config.output.emitln (ast)
    }

}

/**
 * Standard main program for TIL chairmarks that parse and transform.
 */
trait TransformingMain extends ParsingMain {

    def transform (ast : Program) : Program

    override def process (filename : String, ast : Program, config : Config) {
        val newast = transform (ast)
        super.process (filename, newast, config)
    }

}
