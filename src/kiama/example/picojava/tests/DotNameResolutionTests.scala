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
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 * 
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava.tests

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalatest.junit.JUnit3Suite 

class DotNameResolutionTests extends TestCase with JUnit3Suite {

    import kiama.example.picojava.AbstractSyntax._
    import kiama.example.picojava.NameResolution._
    import kiama.example.picojava.TypeAnalysis._

    // For the actual program text, see DotNameResolutionTests.pico

    private val axInA   = Use ("x")
    private val declAAx = VarDecl ("x", Use ("int"))
    private val bxInBB  = Use("x")
    private val byInBB  = Use("y")
    
    val ast =
        Program (Block (
            List (ClassDecl ("A", None, Block (
                      List (VarDecl ("y", Use ("int")),
                            VarDecl ("a", Use ("AA")),
                            AssignStmt (Use ("x"),
                                        Dot (Use ("a"), axInA)),
                            ClassDecl ("AA", None, Block (
                                List (declAAx))),
                            ClassDecl ("BB", Some (Use ("AA")), Block (
                                List (VarDecl ("b", Use ("BB")),
                                      AssignStmt (Dot (Use ("b"), byInBB),
                                                  Dot (Use ("b"), bxInBB)))))))))))

    def testSimpleDot {
        assertSame (declAAx, decl (axInA))
    }

    def testInheritedDot {
        assertSame (declAAx, decl (bxInBB))
    }
    
    def testSurroundingContextIsNotVisible {
        assertTrue (isUnknown (decl (byInBB)))
    }
    
}
