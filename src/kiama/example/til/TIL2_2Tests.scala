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

class TIL2_2Tests extends TestCase with JUnit3Suite with Checkers {
    
    import AST._
    import TIL2_2Main._
            
    /**
     * Simple test of transforming a singleton statement.
     */
    def testForToWhileSingle {
        val input = "for x := 1 to n do write x; end"
        val x = Id ("x")
        val upperx = Id ("Upperx")
        val tree =
            Program (List (
                Decl (x),
                Assign (x, Num (1)),
                Decl (upperx),
                Assign (upperx, Add (Var (Id ("n")), Num (1))),
                While (Sub (Var (x), Var (upperx)),
                    List (
                        Write (Var (x)),
                        Assign (x, Add (Var (x), Num (1)))))))
        test (input, tree)
    }

}
