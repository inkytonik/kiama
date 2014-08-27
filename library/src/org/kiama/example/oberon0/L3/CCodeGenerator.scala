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
package L3

/**
 * C Code generator for the L3 language.
 */
trait CCodeGenerator extends TypeAnalyser with L1.CCodeGenerator with SymbolTable {

    import base.c.{CBlock, CDeclaration, CExpression, CFunctionDecl,
        CInclude, CProgram, CStatement, CVarDecl}
    import base.source.{Block, Declaration, Expression, IdnDef, IdnUse,
        ModuleDecl, Statement}
    import c.{CAddrExp, CAddrType, CCall, CDerefExp, CStrExp, CVoidType}
    import L0.source.IdnExp
    import source.{Call, Mode, ProcDecl, ValMode, VarMode}
    import scala.collection.immutable.Seq

    /**
     * Add STDIO header since output is now possible.
     */
    override def translate (m : ModuleDecl) : CProgram = {
        val CProgram (is, ds) = super.translate (m)
        CProgram (CInclude ("<stdio.h>") +: is, ds)
    }

    /**
     * Add translation of procedure declarations.
     */
    override def translate (d : Declaration) : Seq[CDeclaration] =
        d match {
            case ProcDecl (p @ IdnDef (i), ps, Block (ds, ss), _) =>
                Seq (CFunctionDecl (CVarDecl (mangle (i), CVoidType ()),
                                     translateFormalParams (p),
                                     CBlock (ds flatMap translate,
                                             ss map translate)))
            case _ =>
                super.translate (d)
        }

    /**
     * Translate the formal parameters of a particular defined procedure.
     */
    def translateFormalParams (p : IdnDef): Seq[CDeclaration] =
        parameters (p).get.map {
            case ParamInfo (m, i, t) =>
                translateFormalParam (m, i, t)
        }

    /**
     * Translate a formal parameter into a C parameter variable declaration
     * with the appropriate mode (address for Var, other value).
     */
    def translateFormalParam (m : Mode, i : String, t  : Type) : CDeclaration = {
        val tt = translate (t)
        CVarDecl (mangle (i), if (m == VarMode ()) CAddrType (tt) else tt)
    }

    /**
     * Add translation of call statements.
     */
    override def translate (s : Statement) : CStatement =
        s match {
            case Call (u @ IdnUse (s), ps) =>
                val cps = translateActualParams (u, ps)
                entity (u) match {
                    case _ : BuiltinProc =>
                        s match {
                            case "Read" =>
                                CCall ("scanf", CStrExp ("%d") +: cps)
                            case "Write" =>
                                CCall ("printf", CStrExp (" %d") +: cps)
                            case "WriteLn" =>
                                CCall ("puts", Seq (CStrExp ("")))
                        }
                    case _ =>
                        CCall (mangle (s), cps)
                }
            case _ =>
                super.translate (s)
        }

    /**
     * Translate the actual parameters of a procedure call. Assumes that the
     * right number of parameters are present.
     */
    def translateActualParams (u : IdnUse, ps : Seq[Expression]) : Seq[CExpression] =
        (for (i <- 0 until ps.length)
            yield
                translateActualParam (ps (i), parammode (u, i + 1))
         ).toSeq

    /**
     * Compute an expression for a given actual parameter, inserting an
     * address-of operation for VAR parameters.
     */
    def translateActualParam (p : Expression, mode : Mode) : CExpression = {
        val cpsi = translate (p)
        mode match {
            case VarMode () =>
                cpsi match {
                    case CDerefExp (dcpsi) => dcpsi
                    case _                 => CAddrExp (cpsi)
                }
            case ValMode () =>
                cpsi
        }
    }

    /**
     * Add translation of uses of variable mode parameters in expressions
     * by dereferencing.
     */
    override def translate (e : Expression) : CExpression =
        e match {
            case IdnExp (u @ IdnUse (s)) =>
                val te = super.translate (e)
                entity (u) match {
                    case Parameter (VarMode (), v) => CDerefExp (te)
                    case _                         => te
                }
            case _ =>
                super.translate (e)
        }

}
