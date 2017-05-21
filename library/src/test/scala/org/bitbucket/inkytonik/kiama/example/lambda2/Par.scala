/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

/**
 * Common rules for parallel evaluation methods.
 */
trait Par extends ReduceSubst {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

    /**
     * Reusable strategy for reduction with explicit term-level substitution.
     */
    override lazy val lambda =
        beta + arithop + subsNum + subsVar + subsApp + subsLam + subsOpn + letLet

    /**
     * Beta reduction via term-level substitution.
     */
    override lazy val beta =
        rule[Exp] {
            case App(Lam(x, t, e1), e2) =>
                val y = freshVar()
                Letp(
                    Vector(Bind(y, e2)),
                    Letp(Vector(Bind(x, Var(y))), e1)
                )
        }

    /**
     * Substitution in numeric terms.
     */
    override lazy val subsNum =
        rule[Exp] {
            case Letp(_, e : Num) => e
        }

    /**
     * Lookup a binding for a name in a vector of bindings.
     */
    def lookupb(x : Idn, ds : Vector[Bind]) : Option[Exp] =
        ds.collectFirst {
            case Bind(y, e) if x == y =>
                e
        }

    /**
     * Substitution in variable terms.
     */
    override lazy val subsVar =
        rulefs[Exp] {
            case Letp(ds, e @ Var(x)) =>
                option(lookupb(x, ds)) <+ build(e)
        }

    /**
     * Substitution in applications.
     */
    override lazy val subsApp =
        rule[Exp] {
            case Letp(ds, App(e1, e2)) =>
                App(Letp(ds, e1), Letp(ds, e2))
        }

    /**
     * Substitution in lambda abstractions.
     */
    override lazy val subsLam =
        rule[Exp] {
            case Letp(ds, Lam(x, t, e)) =>
                val y = freshVar()
                Lam(y, t, Letp(ds, Letp(Vector(Bind(x, Var(y))), e)))
        }

    /**
     * Substitution in primitive operations
     */
    override lazy val subsOpn =
        rule[Exp] {
            case Letp(ds, Opn(e1, op, e2)) =>
                Opn(Letp(ds, e1), op, Letp(ds, e2))
        }

    /**
     * Merging two arbitrary parallel binders.
     */
    lazy val letLet =
        rule[Letp] {
            case Letp(ds1, Letp(ds2, e1)) =>
                val ds3 = ds2 map {
                    case Bind(x, e) => Bind(x, Letp(ds1, e))
                }
                val ds4 = ds3 ++ ds1
                Letp(ds4, e1)
        }

}
