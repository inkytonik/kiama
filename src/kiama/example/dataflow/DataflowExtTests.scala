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
 * Tests of extended data flow attribution.
 */
class DataflowExtTests extends TestCase with JUnit3Suite {

    import Dataflow._
    
    use(DataflowFor)
    use(DataflowForeach)
    
    /*
     * begin                 (prog)
     *     y = v             (s1)
     *     z = y             (s2)
     *     x = v             (s3)
     *     foreach (x) do    (s4, s41)
     *         x = w         (s411)
     *         x = v         (s412)
     *     end
     *     return x          (s5)
     * end
     */
    val s1 = Assign ("y", "v")
    val s2 = Assign ("z", "y")
    val s3 = Assign ("x", "v")
    val s411 = Assign ("x", "w")
    val s412 = Assign ("x", "v")
    val s41 = Block (s411, s412)
    val s4 = Foreach ("x", s41)
    val s5 = Return ("x")
    val prog = Block (s1, s2, s3, s4, s5)
    
    def testIn {
        assertEquals (Set ("w", "v"), in (s1))
        assertEquals (Set ("y", "w", "v"), in (s2))
        assertEquals (Set ("w", "v"), in (s3))
        assertEquals (Set ("x", "w", "v"), in (s4))
        assertEquals (Set ("w", "v"), in (s411))
        assertEquals (Set ("w", "v"), in (s412))
        assertEquals (Set ("x"), in (s5))
    }
    
    def testOut {
        assertEquals (Set ("y", "w", "v"), out (s1))
        assertEquals (Set ("w", "v"), out (s2))
        assertEquals (Set ("x", "w", "v"), out (s3))
        assertEquals (Set ("x", "w", "v"), out (s4))
        assertEquals (Set ("w", "v"), out (s411))
        assertEquals (Set ("x", "w", "v"), out (s412))
        assertEquals (Set (), out (s5))
    }
}
