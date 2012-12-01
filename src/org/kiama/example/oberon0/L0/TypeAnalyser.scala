/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package L0

trait TypeAnalyser extends NameAnalyser {

    import base.source.{IdnUse, SourceASTNode}
    import org.kiama.attribution.Attribution.attr
    import org.kiama.util.Messaging.message
    import source.{AndExp, Assignment, ConstDecl, Expression, IdnExp,
        IntExp, NamedType, NegExp, NotExp, OrExp, ProdExpression,
        RelationalExpression, SumExpression, TypeDecl, TypeDef}

    abstract override def check (n : SourceASTNode) {
        n match {
            case e : Expression if !isCompatible (e->tipe, e->exptype) =>
                message (n, "type error: got " + (e->tipe) + ", but expected " + (e->exptype))

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    /**
     * Compatibility of types.  Return true if the type is compatible with the
     * expected type.  Unknown types are compatible with any other type.
     * Otherwise, use look up the base types of what we have and compare them.
     */
    def isCompatible (tipe : Type, exptype : Type) : Boolean =
        (tipe == unknownType) || (exptype == unknownType) ||
            (typebasetype (tipe) == typebasetype (exptype))

    /**
     * The actual type of an expression following type aliases.
     */
    lazy val basetype : Expression => Type =
        attr {
            case e => typebasetype (e->tipe)
        }

    /**
     * The actual type that a user type denotes.
     */
    lazy val typebasetype : Type => Type =
        attr {
            case UserType (_, t2) => typebasetype ((t2.tipe)->deftype)
            case t                => t
        }

    /**
     * The type of an expression.
     */
    lazy val tipe : Expression => Type =
        attr (tipeDef)

    def tipeDef : Expression => Type =
        {
            case _ : RelationalExpression | _ : OrExp | _ : AndExp | _ : NotExp =>
                booleanType

            case _ : SumExpression | _ : ProdExpression | _ : IntExp | _ : NegExp =>
                integerType

            case IdnExp (u : IdnUse) =>
                u->idntype

            case _ =>
                unknownType
        }

    /**
     * The type of the entity denoted by an identifier use.
     */
    lazy val idntype : IdnUse => Type =
        attr (idntypeDef)

    def idntypeDef : IdnUse => Type =
        {
            case u =>
                u->entity match {
                    case Constant (_, ConstDecl (_, e)) => e->tipe
                    case IntegerValue (_, t, _)         => t
                    case Variable (_, t)                => t->deftype
                    case _                              => unknownType
                }
        }

    /**
     * The type given by a type definition.
     */
    lazy val deftype : TypeDef => Type =
        attr (deftypeDef)

    def deftypeDef : TypeDef => Type =
        {
            case NamedType (u : IdnUse) =>
                u->entity match {
                    case t : Type  => t
                    case _         => unknownType
                }
        }

    /**
     * The built-in type associated with a type declaration.
     */
    lazy val decltype : TypeDecl => Type =
        attr {
            case TypeDecl (_, t) => t->deftype
        }

    /**
     * The type expected of an expression as defined by its context.
     */
    lazy val exptype : Expression => Type =
        attr (exptypeDef)

    def exptypeDef : Expression => Type =
        {
            case n =>
                n.parent match {
                    case _ : OrExp | _ : AndExp | _ : NotExp =>
                        booleanType

                    case Assignment (d, _) =>
                        d->tipe

                    case _ =>
                        integerType
                }
        }

}
