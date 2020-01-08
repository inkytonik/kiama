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

/**
 * C Code generator for the L4 language.
 */
trait CCodeGenerator extends TypeAnalyser with L3.CCodeGenerator with SymbolTable {

    import base.source.{Expression, IdnUse}
    import base.c.{CArrayType, CDeclaration, CExpression, CType, CVarDecl}
    import c.{CFieldExp, CIndexExp, CRecordType}
    import L0.source.IdnExp
    import L0.c.CIdnExp
    import L3.source.Mode
    import source.{FieldExp, FieldIdn, IndexExp}

    /**
     * Add translation for array and record types.
     */
    override def translate(t : Type) : CType =
        t match {
            case ArrayType(s, et) =>
                CArrayType(s, translate(et))
            case RecordType(fls) =>
                val vs = fls map {
                    case Field(i, t) =>
                        CVarDecl(i, translate(t))
                }
                CRecordType(vs)
            case _ =>
                super.translate(t)
        }

    /**
     * Uses of array parameter names don't need to be dereferenced.
     */
    override def translate(e : Expression) : CExpression =
        e match {
            case IdnExp(u @ IdnUse(s)) =>
                if (isNotArray(basetype(e)))
                    super.translate(e)
                else
                    CIdnExp(mangle(s))
            case IndexExp(a, i) =>
                CIndexExp(translate(a), translate(i))
            case FieldExp(r, FieldIdn(f)) =>
                CFieldExp(translate(r), f)
            case _ =>
                super.translate(e)
        }

    /**
     * Array formal parameters are not made into address types.
     */
    override def translateFormalParam(m : Mode, i : String, t : Type) : CDeclaration =
        if (isNotArray(typebasetype(t)))
            super.translateFormalParam(m, i, t)
        else {
            val tt = translate(t)
            CVarDecl(mangle(i), tt)
        }

    /**
     * Array parameters get passed by reference, so we don't need to insert
     * addressing operations for VAR.
     */
    override def translateActualParam(p : Expression, mode : Mode) : CExpression =
        if (isNotArray(basetype(p)))
            super.translateActualParam(p, mode)
        else
            translate(p)

}
