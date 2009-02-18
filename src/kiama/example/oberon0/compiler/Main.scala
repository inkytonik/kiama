/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
 * 
 * Contributed by Ben Mockler.
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

package kiama.example.oberon0.compiler

// Object Main
object Main extends Parser
{
    import AST._
    import ErrorCheck._
    import Encoder._
    import kiama.example.oberon0.assembler._
    import kiama.example.oberon0.machine.RISC
    import java.io.FileReader
    import java.io.FileNotFoundException
    
    def main (args: Array[String])
    {
        println ("Input : " + args(0))

        var result : ParseResult[ModuleDecl] = null

        // PARSING
        try {
            val fr : FileReader = new FileReader(args(0))
            result = parseAll(moduledecl, fr)
        }
        catch {
            case exc : FileNotFoundException => println (exc.getMessage)
            return
        }

        result match {
            case Success (mod, in) => {

                val buffer = new StringBuilder
                mod.pretty(buffer, 0)
                println ("Successful parse:")
                println (buffer.toString)
                println ("Position = " + in.pos)

                // SEMANTIC ANALYSIS
                val errs : List[String] = mod->collectErrors

                if(errs.size == 0) {
                    println ("\nNo semantic errors")

                    // Encode
                    Encode(mod)

                    // Assemble
                    val instrs = Assembler.getcode
                    println ("\nInstructions: ")
                    instrs.foreach (instr => println (instr))

                    // Run
                    println ("\nProgram output: ")
                    val mymachine = new RISC (instrs)
                    mymachine.init
                    mymachine.steps
                }
                else {
                    println ("\nSemantic errors occurred:")
                    errs.foreach (err => println (err))
                }
            }

            case f : Failure => println (f) 
        }
    }
}