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

/**
 * Simple dataflow equation attribution example.
 */
object Dataflow {

    import kiama.attribution.Attribution._

    type Var = String
    
    case class Program (body : Stm) extends Attributable
    sealed abstract class Stm extends Attributable
    case class Assign (left : Var, right : Var) extends Stm
    case class While (cond : Var, body : Stm) extends Stm
    case class If (cond : Var, tru : Stm, fls : Stm) extends Stm
    case class Block (stms : Stm*) extends Stm
    case class Return (ret : Var) extends Stm
    case class Empty () extends Stm
    
    /**
     * Control flow successor relation.
     */
    val succ : Stm => Set[Stm] =
        attr {
            case If (_, s1, s2)   => Set (s1, s2)
            case t @ While (_, s) => following (t) + s
            case Return (_)       => Set ()
            case Block (s, _*)    => Set (s)
            case s                => following (s)
        }

    /**
     * Default following statement.
     */
    val following : Stm => Set[Stm] =
        attr {
            case s =>
                s.parent match {
                     case t @ While (_, _)           => Set (t)                                          
                     case b @ Block (_*) if s.isLast => following (b)
                     case Block (_*)                 => Set (s.next)
                     case _                          => Set ()
                }
        }
        
    /**
     * Variable uses.
     */
    val uses : Stm => Set[String] =
        attr {
            case If (v, _, _)  => Set (v)
            case While (v, _)  => Set (v)
            case Assign (_, v) => Set (v)
            case Return (v)    => Set (v)
            case _             => Set ()
        }

    /**
     * Defined variables.
     */
    val defines : Stm => Set[String] =
        attr {
            case Assign (v, _) => Set (v)
            case _             => Set ()
        }
    
    /**
     * Variables "live" into a statement.
     */
    val in : Stm => Set[String] =
        circular (Set[String]()) {
            case s => uses (s) ++ (out (s) -- defines (s))
        }
    
    /**
     * Variables "live" out of a statement.
     */
    val out : Stm => Set[String] =
        circular (Set[String]()) {
            case s => succ (s).flatMap (in) 
        }
        
}
