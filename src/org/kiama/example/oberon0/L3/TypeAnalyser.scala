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
package example.oberon0
package L3

trait TypeAnalyser extends L2.TypeAnalyser with NameAnalyser {

    import base.source.{Identifier, IdnDef, IdnUse, SourceASTNode}
    import L0.source.Expression
    import org.kiama.attribution.Attribution.attr
    import org.kiama.util.Messaging.message
    import org.kiama.util.Patterns.HasParent
    import source.{Call, Mode, ValMode, VarMode}

    abstract override def check (n : SourceASTNode) {
        n match {
            case Call (u @ IdnUse (i), cps) =>
                (u->numparams).foreach (m =>
                    if (m != cps.length)
                        message (n, "wrong number of parameters in call of " + i +
                                    ", expected " + m + ", got " + (cps.length))
                )

            case HasParent (e : Expression, Call (u, _)) =>
                parammode (u, e.index) match {
                    case VarMode () if !isLvalue (e) =>
                        message (e, "illegal VAR parameter")
                    case _ =>
                        // Ok
                }

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    /**
     * Calculate the number of parameters to a procedure.  Return None if
     * the identifier use does not denote a procedure.
     */
    lazy val numparams : IdnUse => Option[Int] =
        attr (
            n =>
                (n->entity) match {
                    case Procedure (_, ps) =>
                        Some (ps.params.foldRight (0) {
                                  case (fps, n) => fps.idndefs.length + n
                              })
                    case BuiltinProc (_, ps) =>
                        Some (ps.length)
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
    lazy val parameters : Identifier => Option[List[ParamInfo]] =
        attr (
            n =>
                (n->entity) match {
                    case b : BuiltinProc =>
                        Some (b.params)
                    case Procedure (_, pd) =>
                        val ps = for (fps <- pd.params; IdnDef (i) <- fps.idndefs)
                                     yield
                                         ParamInfo (fps.mode, i, (fps.tipe)->deftype)
                        Some (ps)
                    case _ =>
                        None
                }
        )

    /**
     * Return the ith parameter type of the procedure denoted by u (counting
     * from one).  If u is not a procedure or has less than i parameters,
     * return an unknown type.
     */
    def paramtype (u : IdnUse, i : Int) : Type =
        (u->parameters) match {
            case Some (ps) if i <= ps.length =>
                ps (i - 1).tipe
            case _ =>
                unknownType
        }

    /**
     * Return the ith parameter mode of the procedure denoted by u (counting
     * from one).  If u is not a procedure or has less than i parameters,
     * return a value mode, since that mode places no constraints on its
     * actual parameter.
     */
    def parammode (u : IdnUse, i : Int) : Mode =
        (u->parameters) match {
            case Some (ps) if i <= ps.length =>
                ps (i - 1).mode
            case _ =>
                ValMode ()
        }

    /**
     * The type of a parameter is the type of its underlying variable.
     */
    override def idntypeDef : IdnUse => Type =
        (u =>
            u->entity match {
                case Parameter (_, Variable (_, t)) =>
                    t->deftype
                case _ =>
                    super.idntypeDef (u)
            })

    /**
     * The expected type of a parameter to a call is the type of the parameter
     * at that position.
     */
    override def exptypeDef : Expression => Type =
        (e =>
            e.parent match {
                case Call (u, _) => paramtype (u, e.index)
                case _           => super.exptypeDef (e)
            })

}
