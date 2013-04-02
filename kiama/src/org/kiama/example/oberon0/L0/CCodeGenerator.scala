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
package L0

/**
 * C Code generator for the L0 language.
 */
trait CCodeGenerator extends base.CCodeGenerator with TypeAnalyser {

    import base.c.{CDeclaration, CExpression, CIntExp, CIntType,
        CStatement, CType, CVarDecl}
    import base.source.{Declaration, IdnDef, IdnUse, Statement}
    import c.{CAddExp, CAndExp, CAssignment, CDivExp, CEqExp, CGeExp,
        CGtExp, CIdnExp, CInitDecl, CLeExp, CLtExp, CModExp, CMulExp,
        CNamedType, CNeExp, CNegExp, CNotExp, COrExp, CSubExp, CTypeDef}
    import source.{AddExp, AndExp, Assignment, ConstDecl, DivExp, EqExp,
        Expression, GeExp, GtExp, IdnExp, IntExp, LeExp, LtExp, ModExp,
        MulExp, NeExp, NegExp, NotExp, OrExp, SubExp, TypeDecl, VarDecl}

    /**
     * Mangle an Oberon name so it is safe to be used at the C level.
     * We assume that there are no C names with "ob_" prefix.
     */
    def mangle (s : String) : String =
        "ob_" + s

    /**
     * Generate C equivalent of a type.
     */
    def translate (t : Type) : CType =
        t match {
            case UserType (i, _) => CNamedType (mangle (i))
            case _               => CIntType ()
        }

    /**
     * Generate C equivalent of a declaration.
     */
    def translate (d : Declaration) : List[CDeclaration] =
        d match {
            case ConstDecl (IdnDef (i), e) =>
                List (CInitDecl (CVarDecl (mangle (i), CIntType ()), CIntExp (e->value)))
            case TypeDecl (IdnDef (i), t) =>
                List (CTypeDef (CVarDecl (mangle (i), translate (t->deftype))))
            case VarDecl (is, td) =>
                val t = td->deftype
                is map {
                    case IdnDef (i) => CVarDecl (mangle (i), translate (t))
                }
        }

    /**
     * Add translation of assignment statements.
     */
    override def translate (s : Statement) : CStatement =
        s match {
            case Assignment (d, e) =>
                CAssignment (translate (d), translate (e))
            case _ =>
                super.translate (s)
        }

    /**
     * Generate C equivalents of expressions.
     */
    def translate (e : Expression) : CExpression =
        e match {
            case EqExp  (l, r) => CEqExp  (translate (l), translate (r))
            case NeExp  (l, r) => CNeExp  (translate (l), translate (r))
            case LtExp  (l, r) => CLtExp  (translate (l), translate (r))
            case LeExp  (l, r) => CLeExp  (translate (l), translate (r))
            case GtExp  (l, r) => CGtExp  (translate (l), translate (r))
            case GeExp  (l, r) => CGeExp  (translate (l), translate (r))
            case AddExp (l, r) => CAddExp (translate (l), translate (r))
            case SubExp (l, r) => CSubExp (translate (l), translate (r))
            case OrExp  (l, r) => COrExp  (translate (l), translate (r))
            case MulExp (l, r) => CMulExp (translate (l), translate (r))
            case DivExp (l, r) => CDivExp (translate (l), translate (r))
            case ModExp (l, r) => CModExp (translate (l), translate (r))
            case AndExp (l, r) => CAndExp (translate (l), translate (r))
            case NegExp (e)    => CNegExp (translate (e))
            case NotExp (e)    => CNotExp (translate (e))
            case IntExp (i)    => CIntExp (i)
            case IdnExp (u @ IdnUse (s)) =>
                (u->entity) match {
                    case IntegerValue (_, _, v) => CIntExp (v)
                    case _                      => CIdnExp (mangle (s))
                }
        }

}
