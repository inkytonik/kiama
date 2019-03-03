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
package example.oberon0
package L0

trait TypeAnalyser extends NameAnalyser {

    import base.source.{Expression, IdnUse, SourceNode}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, message, Messages}
    import source.{
        AndExp,
        Assignment,
        ConstDecl,
        IdnExp,
        IntExp,
        NamedType,
        NegExp,
        NotExp,
        OrExp,
        ProdExpression,
        RelationalExpression,
        SumExpression,
        TypeDecl,
        TypeDef
    }

    /**
     * The error checking for this level.
     */
    override def errorsDef(n : SourceNode) : Messages =
        super.errorsDef(n) ++
            check(n) {
                case e : Expression if !isCompatible(tipe(e), exptype(e)) =>
                    message(e, s"got ${tipe(e)}, but expected ${exptype(e)}")
            }

    /**
     * Compatibility of types.  Return true if the type is compatible with the
     * expected type.  Unknown types are compatible with any other type.
     * Otherwise, use look up the base types of what we have and compare them.
     */
    def isCompatible(tipe : Type, exptype : Type) : Boolean =
        (tipe == unknownType) || (exptype == unknownType) ||
            (typebasetype(tipe) == typebasetype(exptype))

    /**
     * The actual type of an expression following type aliases.
     */
    lazy val basetype : Expression => Type =
        attr(
            e => typebasetype(tipe(e))
        )

    /**
     * The actual type that a user type denotes.
     */
    lazy val typebasetype : Type => Type =
        attr {
            case UserType(_, t2) => typebasetype(deftype(t2.tipe))
            case t               => t
        }

    /**
     * The type of an expression.
     */
    lazy val tipe : Expression => Type =
        attr(tipeDef)

    def tipeDef : Expression => Type =
        {
            case _ : RelationalExpression | _ : OrExp | _ : AndExp | _ : NotExp =>
                booleanType

            case _ : SumExpression | _ : ProdExpression | _ : IntExp | _ : NegExp =>
                integerType

            case IdnExp(u : IdnUse) =>
                idntype(u)

            case _ =>
                unknownType
        }

    /**
     * The type of the entity denoted by an identifier use.
     */
    lazy val idntype : IdnUse => Type =
        attr(idntypeDef)

    def idntypeDef : IdnUse => Type =
        (u =>
            entity(u) match {
                case Constant(_, ConstDecl(_, e)) => tipe(e)
                case IntegerValue(_, t, _)        => t
                case Variable(_, t)               => deftype(t)
                case _                            => unknownType
            })

    /**
     * The type given by a type definition.
     */
    lazy val deftype : TypeDef => Type =
        attr(deftypeDef)

    def deftypeDef : TypeDef => Type =
        {
            case NamedType(u : IdnUse) =>
                entity(u) match {
                    case t : Type => t
                    case _        => unknownType
                }
        }

    /**
     * The built-in type associated with a type declaration.
     */
    lazy val decltype : TypeDecl => Type =
        attr {
            case TypeDecl(_, t) => deftype(t)
        }

    /**
     * The type expected of an expression as defined by its context.
     */
    lazy val exptype : Expression => Type =
        attr(exptypeDef)

    def exptypeDef : Expression => Type =
        {
            case tree.parent(_ : OrExp | _ : AndExp | _ : NotExp) =>
                booleanType

            case tree.parent(Assignment(d, _)) =>
                tipe(d)

            case _ =>
                integerType
        }

}
