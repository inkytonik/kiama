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
 * Lazy evaluation of lambda calculus with parallel term-level substitution
 * and arithmetic operations.
 */
trait ParLazy extends Par {

    import AST._
    import org.kiama.rewriting.Rewriter._
    import scala.collection.mutable.HashMap

    /**
     * Lift an expression to be evaluated to a substitution.
     */
    lazy val letLift =
        rule {
            case e : Exp => Letp (List (), e)
        }

    /**
     * Drop the bindings.
     */
    lazy val letDrop =
        rule {
            case Letp (_, e) => e
        }

    /**
     * Substitute a variable and maintain the bindings.
     */
    override lazy val subsVar =
        rulefs {
            case Letp (ds, Var (x)) =>
                lookupb (x, ds) <* rule { case e : Exp => Letp (ds, e) }
        }

    /**
     * Apply substitutions lazily in an application, maintaining the
     * environment.
     */
    def letAppL (eval : => Strategy) =
        rulefs {
            case Letp (ds1, App (e1, e2)) =>
                eval (Letp (ds1, e1)) <* rule {
                    case Letp (ds2, e3) =>
                        Letp (ds2, App (e3, e2))
                }
        }

    /**
     * Apply substitutions strictly in an operator evaluation, maintaining the
     * environment.
     */
    def letOpn (eval : => Strategy) =
        rulefs {
            case Letp (ds1, Opn (op, e1, e2)) =>
                eval (Letp (ds1, e1)) <* rulefs {
                    case Letp (ds2, e3) =>
                        eval (Letp (ds2, e2)) <* rule {
                            case Letp (ds3, e4) =>
                                Letp (ds3, Opn (op, e3, e4))
                        }
                }
        }

    /**
     * Rename all variables in a parallel binding expression to fresh vars.
     * Assumes that the names are unique to start with.
     */
    def rename : Strategy = {
        val env = new HashMap[Idn,Idn]()
        val newname =
            rule {
                case i : Idn => env.getOrElseUpdate (i, freshvar ())
            }
        val chgname =
            rule {
                case i : Idn => env.getOrElse (i, i)
            }
        lazy val r : Strategy =
            attempt (Var (chgname) + App (r, r) + Lam (newname, id, r) +
                Opn (id, r, r) + Letp (map (r), r) + Bind (newname, r))
        r
    }

    /**
     * Rename variables bound in an inner let (corresponds to heap allocation
     * for these values).
     */
    lazy val letLetRen =
        rulefs {
            case Letp (ds1, Letp (ds2, e1)) =>
                rename (Letp (ds2, e1)) <* rule {
                    case Letp (ds3, e2) =>
                        val ds4 = ds3 ++ ds1
                        Letp (ds4, e2)
                }
        }

}
