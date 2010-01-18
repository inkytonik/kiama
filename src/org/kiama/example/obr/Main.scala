/**
 * Obr language implementation main program.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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

package org.kiama.example.obr

/**
 * Obr language implementation main program.
 */
object Main extends SyntaxAnalysis {

    import java.io.FileReader
    import SemanticAnalysis._
    import Transformation._
    import org.kiama.util.Messaging._

    /**
     * Compile the Obr program in the file given as the first command-line
     * argument and emit code to the console.
     */
    def main (args : Array[String]) {
        driver (args, new Emitter)
    }

    /**
     * Compile the Obr program in the file given as the first command-line
     * argument.  Code is output using the specified emitter.  True is
     * returned if it all worked, otherwise false.
     */
    def driver (args : Array[String], emitter : Emitter) : Boolean = {

          args.size match {

              // If there is exactly one command-line argument
              case 1 =>
                  // Create a reader for the argument file name
                  val reader = new FileReader (args (0))

                  // Parse the file
                  parse (parser, reader) match {
                      // If it worked, we get a source tree
                      case Success (sourcetree, _) =>

                          // Print out the source tree for debugging
                          // println (sourcetree)

                          // Initialise compiler state
                          SymbolTable.reset ()
                          SPARCTree.reset ()

                          // Conduct semantic analysis and report any errors
                          if (sourcetree->errors) {
                              report
                              false
                          } else {
                              // Compile the source tree to a target tree
                              val targettree = sourcetree->code

                              // Print out the target tree for debugging
                              // println (targettree)

                              // Encode the target tree and emit the assembler
                              val e = new Encoder (emitter)
                              e.encode (targettree)
                              true
                          }

                      // Parsing failed, so report it
                      case f =>
                          println (f)
                          false
                  }

              // Complain otherwise
              case _ =>
                  println ("usage: scala obr.Main file.obr")
                  false

        }

    }

}
