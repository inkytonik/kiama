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

package kiama.example.dataflow

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalatest.junit.JUnit3Suite 

/**
 * Tests of data flow attribution.
 */
class DataflowTests extends TestCase with JUnit3Suite {

    import Dataflow._
    
    /*
     * begin                 (prog)
     *     bogus1 = value    (s1)
     *     bogus2 = bogus1   (s2)
     *     x = value         (s3)
     *     while (x) begin   (s4, s41)
     *         x = bogus3    (s411)
     *         x = value     (s412)
     *     end
     *     return x          (s5)
     * end
     */
    val s1 = Assign ("bogus1", "value")
    val s2 = Assign ("bogus2", "bogus1")
    val s3 = Assign ("x", "value")
    val s411 = Assign ("x", "bogus3")
    val s412 = Assign ("x", "value")
    val s41 = Block (s411, s412)
    val s4 = While ("x", s41)
    val s5 = Return ("x")
    val prog = Block (s1, s2, s3, s4, s5)
    
    def testIn {
        assertEquals (Set ("bogus3", "value"), in (s1))
        assertEquals (Set ("bogus1", "bogus3", "value"), in (s2))
        assertEquals (Set ("bogus3", "value"), in (s3))
        assertEquals (Set ("x", "bogus3", "value"), in (s4))
        assertEquals (Set ("bogus3", "value"), in (s411))
        assertEquals (Set ("bogus3", "value"), in (s412))
        assertEquals (Set ("x"), in (s5))
    }
    
    def testOut {
        assertEquals (Set ("bogus1", "bogus3", "value"), out (s1))
        assertEquals (Set ("bogus3", "value"), out (s2))
        assertEquals (Set ("x", "bogus3", "value"), out (s3))
        assertEquals (Set ("x", "bogus3", "value"), out (s4))
        assertEquals (Set ("bogus3", "value"), out (s411))
        assertEquals (Set ("x", "bogus3", "value"), out (s412))
        assertEquals (Set (), out (s5))
    }
    
}
