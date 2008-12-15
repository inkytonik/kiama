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
                                
package kiama.example.til

import junit.framework.TestCase
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers 

class TIL2_1Tests extends TestCase with JUnit3Suite with Checkers {
    
    import AST._
    import TIL2_1Main._

    private val x = Id ("x")
    private val y = Id ("y")
    private val n = Id ("n")
            
    /**
     * Simple test of transforming a singleton statement.
     */
    def testForDeclSingle {
        val input = "for x := 1 to n do write x; end"
        val tree =
            Program (List (
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x))))))
        test (input, tree)
    }
    
    /**
     * Simple test of transforming the first statement of a sequence.
     */
    def testForDeclFirst {
        val input = "for x := 1 to n do write x; end write x;"
        val tree =
            Program (List (
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x)))),
                Write (Var (x))))
        test (input, tree)
    }
    
    /**
     * Simple test of transforming the last statement of a sequence.
     */
    def testForDeclLast {
        val input = "write x; for x := 1 to n do write x; end"
        val tree =
            Program (List (
                Write (Var (x)),
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x))))))
        test (input, tree)
    }

    /**
     * Simple test of transforming a middle statement of a sequence.
     */
    def testForDeclMiddle {
        val input = "write x; for x := 1 to n do write x; end write x;"
        val tree =
            Program (List (
                Write (Var (x)),
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x)))),
                Write (Var (x))))
        test (input, tree)
    }

    /**
     * Simple test of transforming nested statements.
     */
    def testForDeclNested {
        val input = "for x := 1 to n do for y := 0 to x do write y; end end"
        val tree =
            Program (List (
                Decl (x),
                For (x, Num (1), Var (n), List (
                     Decl (y),
                     For (y, Num (0), Var (x), List (
                         Write (Var (y))))))))
        test (input, tree)
    }
    
}
