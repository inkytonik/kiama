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

package kiama.attribution

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalatest.junit.JUnit3Suite 

class AttributionTests extends TestCase with JUnit3Suite {
  
    import Attribution._

    /**
     * Test that attribute definitions are only executed once.
     */
    def testMemoisation {
        
        abstract class Tree extends Attributable
        case class Pair (left : Tree, right : Tree) extends Tree
        case class Leaf (value : Int) extends Tree
       
        var count = 0

        lazy val maximum : Tree => Int =
            attr {
                case Pair (l,r) => count = count + 1; maximum (l).max (maximum (r))
                case Leaf (v)   => v
            }
            
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        assertEquals (10, maximum (t))
        assertEquals (10, maximum (t))
        assertEquals (2, count)
    
    }
  
}
