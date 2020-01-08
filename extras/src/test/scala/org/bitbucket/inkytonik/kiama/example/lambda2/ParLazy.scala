/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

/**
 * Lazy evaluation of lambda calculus with parallel term-level substitution
 * and arithmetic operations.
 */
trait ParLazy extends Par {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Lift an expression to be evaluated to a substitution.
     */
    lazy val letLift =
        rule[Exp] {
            case e => Letp(Vector(), e)
        }

    /**
     * Drop the bindings.
     */
    lazy val letDrop =
        rule[Exp] {
            case Letp(_, e) => e
        }

    /**
     * Substitute a variable and maintain the bindings.
     */
    override lazy val subsVar =
        rulefs[Exp] {
            case Letp(ds, Var(x)) =>
                option(lookupb(x, ds)) <* rule[Exp] { case e => Letp(ds, e) }
        }

    /**
     * Apply substitutions lazily in an application, maintaining the
     * environment.
     */
    def letAppL(eval : => Strategy) : Strategy =
        rulefs[Letp] {
            case Letp(ds1, App(e1, e2)) =>
                option(eval(Letp(ds1, e1))) <* rule[Letp] {
                    case Letp(ds2, e3) =>
                        Letp(ds2, App(e3, e2))
                }
        }

    /**
     * Apply substitutions strictly in an operator evaluation, maintaining the
     * environment.
     */
    def letOpn(eval : => Strategy) : Strategy =
        rulefs[Letp] {
            case Letp(ds1, Opn(e1, op, e2)) =>
                option(eval(Letp(ds1, e1))) <* rulefs[Letp] {
                    case Letp(ds2, e3) =>
                        option(eval(Letp(ds2, e2))) <* rule[Letp] {
                            case Letp(ds3, e4) =>
                                Letp(ds3, Opn(e3, op, e4))
                        }
                }
        }

    /**
     * Rename all variables in a parallel binding expression to fresh vars.
     * Assumes that the names are unique to start with.
     */
    def rename : Strategy = {
        val env = scala.collection.mutable.HashMap[Idn, Idn]()
        val newname =
            rule[Idn] {
                case i => env.getOrElseUpdate(i, freshVar())
            }
        val chgname =
            rule[Idn] {
                case i => env.getOrElse(i, i)
            }
        lazy val r : Strategy =
            attempt(Var(chgname) + App(r, r) + Lam(newname, id, r) +
                Opn(r, id, r) + Letp(map(r), r) + Bind(newname, r))
        r
    }

    /**
     * Rename variables bound in an inner let (corresponds to heap allocation
     * for these values).
     */
    lazy val letLetRen =
        rulefs[Letp] {
            case Letp(ds1, Letp(ds2, e1)) =>
                option(rename(Letp(ds2, e1))) <* rule[Letp] {
                    case Letp(ds3, e2) =>
                        val ds4 = ds3 ++ ds1
                        Letp(ds4, e2)
                }
        }

}
