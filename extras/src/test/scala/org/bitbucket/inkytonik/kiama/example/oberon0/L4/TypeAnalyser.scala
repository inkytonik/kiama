/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L4

trait TypeAnalyser extends L3.TypeAnalyser with SymbolTable {

    import base.Oberon0Entity
    import base.source.{Expression, IdnDef, SourceNode}
    import L0.source.{Assignment, NamedType, TypeDef}
    import L3.source.{FPSection, ValMode}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, error, Messages}
    import source.{ArrayTypeDef, FieldExp, FieldIdn, Fields, IndexExp, RecordTypeDef}

    /**
     * The error checking for this level.
     */
    override def errorsDef(n : SourceNode) : Messages =
        super.errorsDef(n) ++
            check(n) {
                case FPSection(_, _, t) if !t.isInstanceOf[NamedType] =>
                    error(t, "parameter type must be identifier")

                case n @ FPSection(ValMode(), _, t) if !(isNotArray(typebasetype(deftype(t)))) =>
                    error(n, "array parameter must be VAR")

                case n @ FPSection(ValMode(), _, t) if !(isNotRecord(typebasetype(deftype(t)))) =>
                    error(n, "record parameter must be VAR")

                case ArrayTypeDef(s, t) =>
                    error(s, s"ARRAY size is ${value(s)} but should be >= 0",
                        isInteger(tipe(s)) && isconst(s) && (value(s) < 0))

                case Assignment(l, _) if !isNotArray(basetype(l)) =>
                    error(l, "can't assign to array")

                case IndexExp(a, _) if !isArray(basetype(a)) =>
                    error(a, "array indexing attempted on non-ARRAY")

                case IndexExp(a, e) =>
                    val ArrayType(s, _) = basetype(a)
                    error(e, "index out of range",
                        (basetype(e) == integerType) && isconst(e) &&
                            ((value(e) < 0) || (value(e) >= s)))

                case Assignment(l, _) if !isNotRecord(basetype(l)) =>
                    error(l, "can't assign to record")

                case FieldExp(r, f @ FieldIdn(i)) =>
                    val t = basetype(r)
                    if (isRecord(t))
                        error(f, s"record doesn't contain a field called '$i'",
                            !hasField(t, i))
                    else
                        error(f, "field access attempted on non-RECORD")
            }

    override def rootconstexpDef : Expression => Boolean =
        {
            case tree.parent(_ : ArrayTypeDef) =>
                true
            case e =>
                super.rootconstexpDef(e)
        }

    /**
     * Array and record types are only compatible if they have the same name.
     */
    override def isCompatible(tipe : Type, exptype : Type) : Boolean =
        (tipe, exptype) match {
            case (UserType(n1, _), UserType(n2, _)) =>
                n1 == n2
            case _ =>
                super.isCompatible(tipe, exptype)
        }

    override def entityFromDecl(n : IdnDef, i : String) : Oberon0Entity =
        n match {
            case tree.parent(Fields(_, p)) =>
                Field(i, deftype(p))
            case _ =>
                super.entityFromDecl(n, i)
        }

    override def deftypeDef : TypeDef => Type =
        {
            case ArrayTypeDef(s, t) =>
                ArrayType(value(s), deftype(t))
            case RecordTypeDef(fls) =>
                RecordType(fieldListsToFields(fls))
            case n =>
                super.deftypeDef(n)
        }

    def fieldListsToFields(fls : Vector[Fields]) : Vector[Field] =
        (for (fl <- fls) yield {
            val t = deftype(fl.tipe)
            fl.idndefs.map { case f => Field(f, t) }
        }).flatten

    override def tipeDef : Expression => Type =
        {
            case IndexExp(a, _) =>
                basetype(a) match {
                    case ArrayType(_, t) => t
                    case _               => unknownType
                }

            case f @ FieldExp(r, FieldIdn(i)) =>
                basetype(r) match {
                    case RecordType(fs) =>
                        fs.filter(_.ident == i) match {
                            case Vector(f) => f.tipe
                            case _         => unknownType
                        }
                    case _ =>
                        unknownType
                }

            case n =>
                super.tipeDef(n)
        }

    /**
     * Use of arrays and records is dealt with separately, not via the
     * expected type.
     */
    override def exptypeDef : Expression => Type =
        {
            case e @ tree.parent(p : IndexExp) =>
                if (e eq p.base)
                    unknownType
                else
                    integerType
            case tree.parent(_ : FieldExp) =>
                unknownType
            case e =>
                super.exptypeDef(e)
        }

}
