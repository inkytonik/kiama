/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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

package org.kiama.example.dataflow

import DataflowAST._
import org.kiama.util.Compiler

/**
 * Parse a simple imperative language program, calculate its dataflow
 * relations and use them to remove dead assignments.
 */
 class Driver extends Compiler[Stm] with SyntaxAnalyser {

     import java.io.FileReader
     import org.kiama.util.Emitter

     /**
      * The usage message for an erroneous invocation.
      */
     val usage = "usage: scala org.kiama.example.dataflow.Main file.data"

     /**
      * The parser to use to process the input into an AST.
      */
     def parse (filename : String) : Option[Stm] = {
         val reader = new FileReader (filename)
         super.parse (parser, reader) match {
             case Success (ast, _) =>
                 Some (ast)
             case f =>
                 println (f)
                 None
         }
     }

     /**
      * Process the AST by optimising it, then print optimised AST.
      */
     def process (ast : Stm, emitter : Emitter) : Boolean = {
         val optast = Optimise.run (ast)
         emitter.emitln (optast)
         true
     }

 }

 /**
  * Dataflow language implementation main program.
  */
 object Main extends Driver
