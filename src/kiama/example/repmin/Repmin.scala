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
    
package kiama.example.repmin

object Repmin {
  
    import kiama.attribution.Attribution._

    abstract class Tree extends Attributable
    case class Pair (left : Tree, right : Tree) extends Tree
    case class Leaf (value : Int) extends Tree

    val locmin : Tree => Int = 
        attr {
            case Pair (l, r) => locmin (l).min (locmin (r))
            case Leaf (v)    => v
        }  
    
    val globmin : Tree => Int =
        attr {
            case t if t.isRoot => locmin (t)
            case t             => globmin (t.parent.asInstanceOf[Tree]) 
        }
                
    val repmin : Tree => Tree = 
        attr {
            case Pair (l, r)  => Pair (repmin (l), repmin (r))
            case t @ Leaf (_) => Leaf (globmin (t))
        }

}
