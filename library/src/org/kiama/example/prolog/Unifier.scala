/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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

package org.kiama
package example.prolog

import PrologTree._
import org.kiama.rewriting.Rewriter
import org.kiama.rewriting.Rewriter.{ Term => _, _ }
import scala.collection.immutable

/**
 * A substitution that records how variables (given by their names) are to
 * be replaced by Prolog terms.
 */
abstract class Subst {

    self =>

    /**
     * A map to hold the actual substitution pairs.
     */
    val m : immutable.Map[String,Term]

    /**
     * Apply this substitution to a term, returning the resulting term.
     */
    def apply[T] (t : T) : T = {
        val r = everywheretd (rule[Term] {
            case Var (s) if m contains s => m (s)
        })
        rewrite (r) (t)
    }

    /**
     * Compose two substitutions.  Assumes that the domains are disjoint.
     */
    def compose (s : Subst) : Subst =
        new Subst {
            val m = self.m ++ s.m
        }

    /**
     * Create a string representation for debugging.
     */
    override def toString : String =
        m.mkString ("Subst (", ", ", ")")

}

/**
 * Factory and utility methods for substitutions.
 */
object Subst {

    /**
     * Make a substitution from the argument sequence of pairs.
     */
    def apply (l : (String,Term)*) : Subst =
        new Subst {
            val m = immutable.Map (l : _*)
        }

}

object Unifier {

    /**
     * Construct a most-general unifier of two terms.  If the terms
     * unify with substitution s, return Some (s).  Otherwise, return
     * None if the terms cannot be unified.
     */
    def unify (t1 : Term, t2 : Term) : Option[Subst] =
        (t1, t2) match {
            case (Var (s), Var (t)) if s == t =>
                Some (Subst ())
            case (Var (s), t) =>
                Some (Subst ((s, t)))
            case (t, Var (s)) =>
                Some (Subst ((s, t)))
            case (Pred (s, ts), Pred (u, vs)) =>
                if (s == u) unify (ts, vs) else None
            case _ if t1 == t2 =>
                Some (Subst ())
            case _ =>
                None
        }

    /**
     * Construct a most-general unifier of two term lists.  If the lists
     * unify with substitution s, return Some (s).  Otherwise, return
     * None if the lists of terms cannot be unified.
     */
    def unify (ls : Seq[Term], rs : Seq[Term]) : Option[Subst] =
        ls match {
            case Nil =>
                if (rs == Nil) Some (Subst()) else None
            case _ =>
                if (rs == Nil)
                    None
                else
                    unify (ls.head, rs.head) match {
                        case None         => None
                        case Some (subs1) =>
                            val ts2 = subs1 (ls.tail)
                            val vs2 = subs1 (rs.tail)
                            unify (ts2, vs2) match {
                                case None         => None
                                case Some (subs2) => Some (subs1 compose subs2)
                            }
                    }
        }

}
