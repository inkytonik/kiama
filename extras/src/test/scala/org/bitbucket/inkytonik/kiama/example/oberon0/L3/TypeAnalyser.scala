/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L3

trait TypeAnalyser extends L2.TypeAnalyser with NameAnalyser {

    import base.source.{Expression, Identifier, IdnDef, IdnUse, SourceNode}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, error, Messages, noMessages}
    import source.{Call, Mode, ValMode, VarMode}

    /**
     * The error checking for this level.
     */
    override def errorsDef(n : SourceNode) : Messages =
        super.errorsDef(n) ++
            check(n) {
                case Call(u @ IdnUse(i), cps) =>
                    check(numparams(u)) {
                        case Some(m) =>
                            error(u, s"wrong number of parameters in call of $i, expected $m, got ${cps.length}", m != cps.length)
                    }

                case tree.parent.pair(e : Expression, Call(u, _)) =>
                    parammode(u, tree.index(e)) match {
                        case VarMode() if !isLvalue(e) =>
                            error(e, "illegal VAR parameter")
                        case _ =>
                            noMessages
                    }
            }

    /**
     * Calculate the number of parameters to a procedure.  Return None if
     * the identifier use does not denote a procedure.
     */
    lazy val numparams : IdnUse => Option[Int] =
        attr(
            n =>
                entity(n) match {
                    case Procedure(_, ps) =>
                        Some(ps.params.foldRight(0) {
                            case (fps, n) => fps.idndefs.length + n
                        })
                    case BuiltinProc(_, ps) =>
                        Some(ps.length)
                    case _ =>
                        None
                }
        )

    /**
     * Calculate the parameter information list for a procedure.  If it's
     * a built-in we have the information already, otherwise we need to
     * work it out from the declaration.  Returns None if the entity is
     * not a procedure.
     */
    lazy val parameters : Identifier => Option[Vector[ParamInfo]] =
        attr(
            n =>
                entity(n) match {
                    case b : BuiltinProc =>
                        Some(b.params)
                    case Procedure(_, pd) =>
                        val ps = for (fps <- pd.params; IdnDef(i) <- fps.idndefs)
                            yield ParamInfo(fps.mode, i, deftype(fps.tipe))
                        Some(ps)
                    case _ =>
                        None
                }
        )

    /**
     * Return the ith parameter type of the procedure denoted by u (counting
     * from one).  If u is not a procedure or has less than i parameters,
     * return an unknown type.
     */
    def paramtype(u : IdnUse, i : Int) : Type =
        parameters(u) match {
            case Some(ps) if i <= ps.length =>
                ps(i - 1).tipe
            case _ =>
                unknownType
        }

    /**
     * Return the ith parameter mode of the procedure denoted by u (counting
     * from one).  If u is not a procedure or has less than i parameters,
     * return a value mode, since that mode places no constraints on its
     * actual parameter.
     */
    def parammode(u : IdnUse, i : Int) : Mode =
        parameters(u) match {
            case Some(ps) if i <= ps.length =>
                ps(i - 1).mode
            case _ =>
                ValMode()
        }

    /**
     * The type of a parameter is the type of its underlying variable.
     */
    override def idntypeDef : IdnUse => Type =
        (u =>
            entity(u) match {
                case Parameter(_, Variable(_, t)) =>
                    deftype(t)
                case _ =>
                    super.idntypeDef(u)
            })

    /**
     * The expected type of a parameter to a call is the type of the parameter
     * at that position.
     */
    override def exptypeDef : Expression => Type =
        {
            case e @ tree.parent(Call(u, _)) =>
                paramtype(u, tree.index(e))
            case e =>
                super.exptypeDef(e)
        }

}
