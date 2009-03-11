/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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

/**
 * This file is derived from specifications in "Java and the Java Virtual
 * Machine: Definition, Verification and Validation" by Robert Stärk, Joachim
 * Schmid and Egon Börger, Springer, 2001.
 */
 
package kiama.example.javaasm.jvm

import JVMIISA._
import kiama.machine.Machine

/**
 * Java Virtual Machine subset that can execute JavaI programs.
 */
class JVMI (code : Code) extends Machine ("JVMI") {
    
    import Primitives._
    import util.JList
    import util.JList._
    
    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during execution of the machine.
     */
    override def debug = false

    /**
     * The type of the program counter.
     */
    type Pc = Int
    
    /**
     * Program counter.
     */
    val pc = State[Pc] ("pc")

    /**
     * Register file.
     */
    val reg = State[Map[RegNo, Word]] ("reg")

    /**
     * Operand stack.
     */
    val opd = State[JList[Word]] ("opd")
    
    /**
     * Halt flag.  Undefined until the machine is to stop executing.
     */
    val halt = State[String] ("halt")
    
    /**
     * Initialise the state.
     */
    def init {
        pc.update (0)
        reg.update (Map ())
        opd.update (JList[Word] ())
        halt.undefine
    }

    /**
     * The main rule of this machine.
     */
    def main = trustfulVMI

    /**
     * Execution of the JVMI program given by code.  Stop if and when
     * halt becomes defined.
     */
    def trustfulVMI =
        if (halt isUndefined)
            execVMI (code (pc))
        
    /**
     * Execute a single instruction of the JVMI.
     */
    def execVMI (instr : Instr) = {
        if (debug)
            println (name + " exec: " + instr)
        instr match {
            case Prim (p)      => val (opd2, ws) = split (opd, argSize (p))
                                  if ((!isDivMod (p) || (ws (0) != 0))) {
                                      opd := opd2
                                      pc := pc + 1
                                  }
            case Dupx (s1, s2) => val (opd2, JList (ws1, ws2)) = splits (opd, JList (s1, s2))
                                  opd := opd2 ++ ws2 ++ ws1 ++ ws2
                                  pc := pc + 1
            case Pop (s)       => val (opd2, ws) = split (opd, s)
                                  opd := opd2
                                  pc := pc + 1
            case Load (t, x)   => if (size (t) == 1) opd := opd ++ JList (reg (x))
                                  else opd := opd ++ JList (reg (x), reg (x + 1))
                                  pc := pc + 1
            case Store (t, x)  => val (opd2, ws) = split (opd, size (t))
                                  if (size (t) == 1) reg := reg + ((x, ws (0)))
                                  else reg := reg + ((x, ws (0)), (x+1, ws (1)))
                                  opd := opd2
                                  pc := pc + 1
            case Goto (o)      => pc := o
            case Cond (p, o)   => val (opd2, ws) = split (opd, argSize (p))
                                  opd := opd2
                                  if (JVMSBool (p, ws)) pc := o else pc := pc + 1
            case Halt          => halt := "Halt"
        }
    }
        
}
