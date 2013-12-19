/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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

/**
 * Abstract syntax tree representation of goals.
 */
object GoalTree {

    import PrologTree._

    /**
     * Goals.
     */
    sealed trait Goal

    /**
     * Display goal.
     */
    case class DisplayGoal (ps : Set[(String,Term)]) extends Goal

    /**
     * Term goal.
     */
    case class TermGoal (term : Term) extends Goal

    /**
     * Unification goal (written as "left ?= right" in notes).
     */
    case class UnifyGoal (left : Term, right : Term) extends Goal

    /**
     * Match goal (written as "left == right" in notes).
     */
    case class MatchGoal (left : Term, right : Term) extends Goal

}

class Interpreter {

    import GoalTree._
    import PrologTree._
    import Unifier._
    import org.kiama.rewriting.Rewriter.{ Term => _, _ }
    import org.kiama.util.{Counter, Emitter}

    /**
     * Counter used to produce unique names.
     */
    val uniqueNameCounter = new Counter

    /**
     * Rename all of the variables in `t` to a unique name by appending
     * a unique count to the old name.
     */
    def rename[T <: PrologTree] (t : T) : T = {
        val count = uniqueNameCounter.next ()
        val r = everywheretd (rule[Var] {
            case Var (s) =>
                Var (s + count)
        })
        rewrite (r) (t)
    }

    /**
     * Interpret a query against the given program.
     */
    def interpret (query : Term, program : Program, emitter : Emitter) {

        // Get the program clauses.  We reverse them so that when we use
        // them we push onto the goal stack in the correct order, so that
        // earlier ones have priority.
        val clauses = program.cs.reverse

        /**
         * Return a list of the variables names used in the given term.
         */
        def varsof (t : Term) : Set[String] =
            t match {
                case Var (s)      => Set (s)
                case Pred (_, ts) => ts.foldLeft (Set.empty[String]) {
                                         case (s, t) =>
                                             s.union (varsof (t))
                                     }
                case _            => Set.empty
            }

        /**
         * Find the variables in the query.  These are the ones we want
         * to print.
         */
        val goalvars : Set[(String,Term)] =
            for (v <- varsof (query))
                yield {
                    (v, Var (v))
                }

        /**
         * The goal list stack.  Each entry is a list of goals that are to be
         * satisfied. We start with the query and the display goal.
         */
        val glstack =
            scala.collection.mutable.Stack[Seq[Goal]] (
                Seq (TermGoal (query), DisplayGoal (goalvars))
            )

        /**
         * Loop until the interpretation is done, indicated by an empty goal
         * list stack.
         */
        while (glstack.nonEmpty) {

            // Print the stack for debugging
            // emitter.emitln (glstack)
            // emitter.emitln

            // Get the top goal list from the goal list stack
            val gl = glstack.pop ()

            // Decide what to do based on the first goal in the top goal list
            gl (0) match {

                /**
                 * A term that we will try to unify against the heads of each of the
                 * clauses in the program.
                 */
                case TermGoal (l) =>
                    for (clause <- clauses) {
                        val c = rename (clause)
                        val bdygoals = c.bdy map TermGoal
                        glstack.push (UnifyGoal (l, c.hd) +: (bdygoals ++ gl.tail))
                    }

                /**
                 * A goal to unify the two terms and carry on with the tail of the
                 * goal list if the unification was successful.  Discard the current
                 * goal list if the unification fails.
                 */
                case UnifyGoal (l, r) =>
                    unify (l, r) match {
                        case Some (subst) =>
                            glstack.push (subst (gl.tail))
                        case None =>
                            // Do nothing
                    }

                /**
                 * The record of a successful unification.  Carry on with the tail
                 * of the goal list.
                 */
                case MatchGoal (l, r) =>
                    glstack.push (gl.tail)

                /**
                 * Success! We're down to the display goal, so we've successfully
                 * satisfied the query.  The variables in the display goal can be
                 * printed to display the solution.  If there aren't any variables,
                 * just say "yes".
                 */
                case DisplayGoal (ps) =>
                    if (ps.isEmpty)
                        emitter.emitln ("yes")
                    else {
                        val output = ps.map { case (s, v) => s"$s = $v" }
                        emitter.emitln (output.mkString (" "))
                    }

            }
        }

    }

}
