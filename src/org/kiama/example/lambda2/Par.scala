/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
package example.lambda2

/**
 * Common rules for parallel evaluation methods.
 */
trait Par extends ReduceSubst {

    import AST._
    import org.kiama.rewriting.Rewriter._

    /**
     * Reusable strategy for reduction with explicit term-level substitution.
     */
    override lazy val lambda =
        beta + arithop + subsNum + subsVar + subsApp + subsLam + subsOpn + letLet

    /**
     * Beta reduction via term-level substitution.
     */
    override lazy val beta =
        rule {
            case App (Lam (x, t, e1), e2) =>
                val y = freshvar ()
                Letp (List (Bind (y, e2)),
                      Letp (List (Bind (x, Var (y))), e1))
        }

    /**
     * Substitution in numeric terms.
     */
    override lazy val subsNum =
        rule {
            case Letp (_, e : Num) => e
        }

    /**
     * Lookup a binding for a name in a list of bindings.
     */
    def lookupb (x : Idn, ds : List[Bind]) : Option[Exp] = {
        for (Bind (y, e) <- ds if x == y)
            return Some (e)
        None
    }

    /**
     * Substitution in variable terms.
     */
    override lazy val subsVar =
        rulefs {
            case Letp (ds, e @ Var (x)) =>
                lookupb (x, ds) <+ e
        }

    /**
     * Substitution in applications.
     */
    override lazy val subsApp =
        rule {
            case Letp (ds, App (e1, e2)) =>
                App (Letp (ds, e1), Letp (ds, e2))
        }

    /**
     * Substitution in lambda abstractions.
     */
    override lazy val subsLam =
        rule {
            case Letp (ds, Lam (x, t, e)) =>
                val y = freshvar ()
                Lam (y, t, Letp (ds, Letp (List (Bind (x, Var (y))), e)))
        }

    /**
     * Substitution in primitive operations
     */
    override lazy val subsOpn =
        rule {
            case Letp (ds, Opn (op, e1, e2)) =>
                Opn (op, Letp (ds, e1), Letp (ds, e2))
        }

    /**
     * Merging two arbitrary parallel binders.
     */
    lazy val letLet =
        rule {
            case Letp (ds1, Letp (ds2, e1)) =>
                val ds3 = ds2 map {
                    case Bind (x, e) => Bind (x, Letp (ds1, e))
                }
                val ds4 = ds3 ++ ds1
                Letp (ds4, e1)
        }

}
