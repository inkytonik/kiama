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

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers 

/**
 * Tests of the JVMI subset.
 */
class JVMITests extends TestCase with JUnit3Suite with Checkers {
    
    import JVMIISA._
    import Primitives._

    /**
     * Test the compiled code of a simple conditional expression
     * and assignment a = x && (z = y) where all variables are 
     * Boolean (p. 145 of Stärk et al).
     */
    def testCond () {
        // Register assignments
        val x = 0
        val y = 1
        val z = 2
        val a = 3
        
        val code : Code =
            List (Load (IntType, x),    // 0
                  Cond (ifne, 3),       // 1
                  Goto (7),             // 2
                  Load (IntType, y),    // 3
                  Dupx (0, 1),          // 4
                  Store (IntType, z),   // 5
                  Cond (ifne, 10),      // 6
                  Load (IntType, x),    // 7
                  Store (IntType, a),   // 8
                  Goto (12),            // 9
                  Load (IntType, z),    // 10
                  Store (IntType, a),   // 11
                  Halt)                 // 12

        val mc = new JVMI (code)
        
        // z = 0
        // a = 1 && (z = 1) => 1
        mc.init
        mc.reg.update (Map (x -> 1, y -> 1, z -> 0))
        mc.steps
        println (mc.reg.value)
        assertEquals (1, mc.reg.value (a))
        assertEquals (1, mc.reg.value (z))
        
        // z = 0
        // a = 0 && (z = 1) => 0
        mc.init
        mc.reg.update (Map (x -> 0, y -> 1, z -> 0))
        mc.steps
        assertEquals (0, mc.reg.value (a))
        assertEquals (0, mc.reg.value (z))
        
        // z = 1
        // a = 0 && (z = 0) => 0
        mc.init
        mc.reg.update (Map (x -> 0, y -> 0, z -> 1))
        mc.steps
        assertEquals (0, mc.reg.value (a))
        assertEquals (1, mc.reg.value (z))
    }
    
}
