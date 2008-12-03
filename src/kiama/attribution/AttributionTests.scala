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
  
    abstract class Tree extends Attributable
    case class Pair (left : Tree, right : Tree) extends Tree
    case class Leaf (value : Int) extends Tree

    /**
     * Test that cached attribute definitions are only executed once.
     */
    def testCachedAttributes {        
        import Attribution._
        
        var count = 0

        lazy val maximum : Tree ==> Int =
            attr {
                case Pair (l,r) => count = count + 1; (l->maximum).max (r->maximum)
                case Leaf (v)   => v
            }
            
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        assertEquals (10, t->maximum)
        assertEquals (10, t->maximum)
        assertEquals (2, count)    
    }
    
    /**
     * Test that uncached attribute definitions are executed each time.
     */
    def testUncachedAttributes {        
        import UncachedAttribution._
               
        var count = 0

        lazy val maximum : Tree ==> Int =
            attr {
                case Pair (l,r) => count = count + 1; (l->maximum).max (r->maximum)
                case Leaf (v)   => v
            }
            
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))

        assertEquals (10, t->maximum)
        assertEquals (10, t->maximum)
        assertEquals (4, count)    
    }
    
    /**
     * Test that circularities are detected when caching.
     */
    def testCachedCircularity {
        import Attribution._

        lazy val direct : Tree ==> Int =
            attr {
                case t => t->direct
            }
        lazy val indirect : Tree ==> Int =
            attr {
                case t => t->indirect2
            }
        lazy val indirect2 : Tree ==> Int =
            attr {
                case t => t->indirect
            }

        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))            
            
        try {
            t->direct
            fail ("direct circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
        try {
            t->indirect
            fail ("indirect circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
    }
    
    /**
     * Test that circularities are detected when not caching.
     */
    def testUncachedDirectCircularity {
        import UncachedAttribution._

        lazy val direct : Tree ==> Int =
            attr {
                case t => t->direct
            }
        lazy val indirect : Tree ==> Int =
            attr {
                case t => t->indirect2
            }
        lazy val indirect2 : Tree ==> Int =
            attr {
                case t => t->indirect
            }
            
        val t = Pair (Leaf (3), Pair (Leaf (1), Leaf (10)))            

        try {
            t->direct
            fail ("direct circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
        try {
            t->indirect
            fail ("indirect circular computation finished without exception")
        } catch {
            case e : IllegalStateException =>
                // succeed
        }
    }

  
}
