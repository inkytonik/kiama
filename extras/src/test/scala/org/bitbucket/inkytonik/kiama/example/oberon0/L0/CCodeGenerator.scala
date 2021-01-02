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
package L0

/**
 * C Code generator for the L0 language.
 */
trait CCodeGenerator extends TypeAnalyser with base.CCodeGenerator with SymbolTable {

    import base.c.{
        CDeclaration,
        CExpression,
        CIntExp,
        CIntType,
        CStatement,
        CType,
        CVarDecl
    }
    import base.source.{Declaration, Expression, IdnDef, IdnUse, Statement}
    import c.{
        CAddExp,
        CAndExp,
        CAssignment,
        CDivExp,
        CEqExp,
        CGeExp,
        CGtExp,
        CIdnExp,
        CInitDecl,
        CLeExp,
        CLtExp,
        CModExp,
        CMulExp,
        CNamedType,
        CNeExp,
        CNegExp,
        CNotExp,
        COrExp,
        CSubExp,
        CTypeDef
    }
    import source.{
        AddExp,
        AndExp,
        Assignment,
        ConstDecl,
        DivExp,
        EqExp,
        GeExp,
        GtExp,
        IdnExp,
        IntExp,
        LeExp,
        LtExp,
        ModExp,
        MulExp,
        NeExp,
        NegExp,
        NotExp,
        OrExp,
        SubExp,
        TypeDecl,
        VarDecl
    }

    /**
     * Mangle an Oberon name so it is safe to be used at the C level.
     * We assume that there are no C names with "ob_" prefix.
     */
    def mangle(s : String) : String =
        s"ob_$s"

    /**
     * Generate C equivalent of a type.
     */
    def translate(t : Type) : CType =
        t match {
            case UserType(i, _) => CNamedType(mangle(i))
            case _              => CIntType()
        }

    /**
     * Generate C equivalent of a declaration.
     */
    def translate(d : Declaration) : Vector[CDeclaration] =
        d match {
            case ConstDecl(IdnDef(i), e) =>
                Vector(CInitDecl(CVarDecl(mangle(i), CIntType()), CIntExp(value(e))))
            case TypeDecl(IdnDef(i), t) =>
                Vector(CTypeDef(CVarDecl(mangle(i), translate(deftype(t)))))
            case VarDecl(is, td) =>
                val t = deftype(td)
                is map {
                    case IdnDef(i) =>
                        CVarDecl(mangle(i), translate(t))
                }
            case _ =>
                sys.error(s"translate: unexpected Declaration $d")
        }

    /**
     * Add translation of assignment statements.
     */
    override def translate(s : Statement) : CStatement =
        s match {
            case Assignment(d, e) =>
                CAssignment(translate(d), translate(e))
            case _ =>
                super.translate(s)
        }

    /**
     * Generate C equivalents of expressions.
     */
    def translate(e : Expression) : CExpression =
        e match {
            case EqExp(l, r)  => CEqExp(translate(l), translate(r))
            case NeExp(l, r)  => CNeExp(translate(l), translate(r))
            case LtExp(l, r)  => CLtExp(translate(l), translate(r))
            case LeExp(l, r)  => CLeExp(translate(l), translate(r))
            case GtExp(l, r)  => CGtExp(translate(l), translate(r))
            case GeExp(l, r)  => CGeExp(translate(l), translate(r))
            case AddExp(l, r) => CAddExp(translate(l), translate(r))
            case SubExp(l, r) => CSubExp(translate(l), translate(r))
            case OrExp(l, r)  => COrExp(translate(l), translate(r))
            case MulExp(l, r) => CMulExp(translate(l), translate(r))
            case DivExp(l, r) => CDivExp(translate(l), translate(r))
            case ModExp(l, r) => CModExp(translate(l), translate(r))
            case AndExp(l, r) => CAndExp(translate(l), translate(r))
            case NegExp(e)    => CNegExp(translate(e))
            case NotExp(e)    => CNotExp(translate(e))
            case IntExp(i)    => CIntExp(i)
            case IdnExp(u @ IdnUse(s)) =>
                entity(u) match {
                    case IntegerValue(_, _, v) => CIntExp(v)
                    case _                     => CIdnExp(mangle(s))
                }
            case _ =>
                sys.error(s"translate: unexpected Expression $e")
        }

}
