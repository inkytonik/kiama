/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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

/**
 * C Code generator for the L4 language.
 */
trait CCodeGenerator extends L3.CCodeGenerator with SymbolTable with TypeAnalyser {

    import base.source.{Declaration, Expression, IdnDef, IdnUse}
    import base.c.{CArrayType, CDeclaration, CExpression, CType, CVarDecl}
    import c.{CFieldExp, CIndexExp, CRecordType}
    import L0.source.IdnExp
    import L0.c.CIdnExp
    import L3.source.Mode
    import scala.collection.immutable.Seq
    import source.{FieldExp, FieldIdn, IndexExp}

    /**
     * Add translation for array and record types.
     */
    override def translate (t : Type) : CType =
        t match {
            case ArrayType (s, et) =>
                CArrayType (s, translate (et))
            case RecordType (fls) =>
                val vs = fls map {
                             case Field (i, t) =>
                                 CVarDecl (i, translate (t))
                         }
                CRecordType (vs)
            case _ =>
                super.translate (t)
        }

    /**
     * Uses of array parameter names don't need to be dereferenced.
     */
    override def translate (e : Expression) : CExpression =
        e match {
            case IdnExp (u @ IdnUse (s)) =>
                if (isNotArray (basetype (e)))
                    super.translate (e)
                else
                    CIdnExp (mangle (s))
            case IndexExp (a, i) =>
                CIndexExp (translate (a), translate (i))
            case FieldExp (r, FieldIdn (f)) =>
                CFieldExp (translate (r), f)
            case _ =>
                super.translate (e)
        }

    /**
     * Array formal parameters are not made into address types.
     */
    override def translateFormalParam (m : Mode, i : String, t  : Type) : CDeclaration =
        if (isNotArray (typebasetype (t)))
            super.translateFormalParam (m, i, t)
        else {
            val tt = translate (t)
            CVarDecl (mangle (i), tt)
        }

    /**
     * Array parameters get passed by reference, so we don't need to insert
     * addressing operations for VAR.
     */
    override def translateActualParam (p : Expression, mode : Mode) : CExpression =
        if (isNotArray (basetype (p)))
            super.translateActualParam (p, mode)
        else
            translate (p)

}

