/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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
package L4

trait TypeAnalyser extends L3.TypeAnalyser with SymbolTable {

    import base.source.{Expression, IdnDef, SourceNode}
    import L0.source.{Assignment, NamedType, TypeDef}
    import L3.source.{FPSection, ValMode}
    import org.kiama.util.Entity
    import org.kiama.util.Messaging.{check, message, Messages}
    import scala.collection.immutable.Seq
    import source.{ArrayTypeDef, FieldExp, FieldIdn, FieldList, IndexExp,
        RecordTypeDef}

    /**
     * The error checking for this level.
     */
    override def errorsDef (n : SourceNode) : Messages =
        super.errorsDef (n) ++
        check (n) {
            case FPSection (_, _, t) if !t.isInstanceOf[NamedType] =>
                message (t, "parameter type must be identifier")

            case n @ FPSection (ValMode (), _, t) if !(isNotArray (typebasetype (deftype (t)))) =>
                message (n, "array parameter must be VAR")

            case n @ FPSection (ValMode (), _, t) if !(isNotRecord (typebasetype (deftype (t)))) =>
                message (n, "record parameter must be VAR")

            case ArrayTypeDef (s, t) =>
                message (s, s"ARRAY size is ${value (s)} but should be >= 0",
                         isInteger (tipe (s)) && isconst (s) && (value (s) < 0))

            case Assignment (l, _) if ! isNotArray (basetype (l)) =>
                message (l, "can't assign to array")

            case IndexExp (a, _) if ! isArray (basetype (a)) =>
                message (a, "array indexing attempted on non-ARRAY")

            case IndexExp (a, e) =>
                val ArrayType (s, _) = basetype (a)
                message (e, "index out of range",
                         (basetype (e) == integerType) && isconst (e) &&
                             ((value (e) < 0) || (value (e) >= s)))

            case Assignment (l, _) if ! isNotRecord (basetype (l))  =>
                message (l, "can't assign to record")

            case FieldExp (r, f @ FieldIdn (i)) =>
                val t = basetype (r)
                if (isRecord (t))
                    message (f, s"record doesn't contain a field called '$i'",
                             !hasField (t, i))
                else
                    message (f, "field access attempted on non-RECORD")
        }

    override def rootconstexpDef : Expression => Boolean =
        {
            case tree.parent (_ : ArrayTypeDef) =>
                true
            case e =>
                super.rootconstexpDef (e)
        }

    /**
     * Array and record types are only compatible if they have the same name.
     */
    override def isCompatible (tipe : Type, exptype : Type) : Boolean =
        (tipe, exptype) match {
            case (UserType (n1, _), UserType (n2, _)) =>
                n1 == n2
            case _ =>
                super.isCompatible (tipe, exptype)
        }

    override def entityFromDecl (n : IdnDef, i : String) : Entity =
        n match {
            case tree.parent (FieldList (_, p)) =>
                Field (i, deftype (p))
            case _ =>
                super.entityFromDecl (n, i)
        }

    override def deftypeDef : TypeDef => Type =
        {
            case ArrayTypeDef (s, t) =>
                ArrayType (value (s), deftype (t))
            case RecordTypeDef (fls) =>
                RecordType (fieldListsToFields (fls))
            case n =>
                super.deftypeDef (n)
        }

    def fieldListsToFields (fls : Seq[FieldList]) : Seq[Field] =
        (for (fl <- fls)
            yield {
                val t = deftype (fl.tipe)
                fl.idndefs.map { case f => Field (f, t) }
            }).flatten

    override def tipeDef : Expression => Type =
        {
            case IndexExp (a, _) =>
                basetype (a) match {
                    case ArrayType (_, t) => t
                    case _                => unknownType
                }

            case f @ FieldExp (r, FieldIdn (i)) =>
                basetype (r) match {
                    case RecordType (fs) =>
                        fs.filter (_.ident == i) match {
                            case Seq (f) => f.tipe
                            case _       => unknownType
                        }
                    case _ =>
                        unknownType
                }

            case n =>
                super.tipeDef (n)
        }

    /**
     * Use of arrays and records is dealt with separately, not via the
     * expected type.
     */
    override def exptypeDef : Expression => Type =
        {
            case tree.parent.pair (e, p : IndexExp) =>
                if (e eq p.base)
                    unknownType
                else
                    integerType
            case tree.parent (_ : FieldExp) =>
                unknownType
            case e =>
                super.exptypeDef (e)
        }

}
