/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.prolog

import PrologTree._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{Term => _, _}
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
    val m : immutable.Map[String, Term]

    /**
     * Apply this substitution to a term, returning the resulting term.
     */
    def apply[T](t : T) : T = {
        val r = everywheretd(rule[Term] {
            case Var(s) if m contains s => m(s)
        })
        rewrite(r)(t)
    }

    /**
     * Compose two substitutions.  Assumes that the domains are disjoint.
     */
    def compose(s : Subst) : Subst =
        new Subst {
            val m = self.m ++ s.m
        }

    /**
     * Create a string representation for debugging.
     */
    override def toString : String =
        m.mkString("Subst (", ", ", ")")

}

/**
 * Factory and utility methods for substitutions.
 */
object Subst {

    /**
     * Make a substitution from the argument sequence of pairs.
     */
    def apply(l : (String, Term)*) : Subst =
        new Subst {
            val m = immutable.Map(l : _*)
        }

}

object Unifier {

    /**
     * Construct a most-general unifier of two terms.  If the terms
     * unify with substitution s, return Some (s).  Otherwise, return
     * None if the terms cannot be unified.
     */
    def unify(t1 : Term, t2 : Term) : Option[Subst] =
        (t1, t2) match {
            case (Var(s), Var(t)) if s == t =>
                Some(Subst())
            case (Var(s), t) =>
                Some(Subst((s, t)))
            case (t, Var(s)) =>
                Some(Subst((s, t)))
            case (Pred(s, ts), Pred(u, vs)) =>
                if (s == u) unify(ts, vs) else None
            case _ if t1 == t2 =>
                Some(Subst())
            case _ =>
                None
        }

    /**
     * Construct a most-general unifier of two term lists.  If the lists
     * unify with substitution s, return Some (s).  Otherwise, return
     * None if the lists of terms cannot be unified.
     */
    def unify(ls : Vector[Term], rs : Vector[Term]) : Option[Subst] =
        ls match {
            case Vector() =>
                if (rs.isEmpty) Some(Subst()) else None
            case _ =>
                if (rs.isEmpty)
                    None
                else
                    unify(ls.head, rs.head) match {
                        case None => None
                        case Some(subs1) =>
                            val ts2 = subs1(ls.tail)
                            val vs2 = subs1(rs.tail)
                            unify(ts2, vs2) match {
                                case None        => None
                                case Some(subs2) => Some(subs1 compose subs2)
                            }
                    }
        }

}
