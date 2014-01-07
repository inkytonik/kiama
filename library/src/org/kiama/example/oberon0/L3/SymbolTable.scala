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

trait SymbolTable extends L0.SymbolTable {

    import org.kiama.util.Entity
    import source.{Mode, ProcDecl, ValMode, VarMode}
    import scala.collection.immutable.Seq

    /**
     * A procedure entity represented by a procedure declaration.
     */
    case class Procedure (ident : String, decl : ProcDecl) extends NamedEntity

    /**
     * A parameter is a variable augmented with a passing mode.
     */
    case class Parameter (mode : Mode, varr : Variable) extends NamedEntity {
        def ident : String = varr.ident
    }

    /**
     * Parameters are variables too.
     */
    override def isVariable (e : Entity) : Boolean =
        super.isVariable (e) || e.isInstanceOf[Parameter]

    /**
     * Information about a particular parameter.  Similar to Parameter but the type
     * has been replaced with its definition.
     */
    case class ParamInfo (mode : Mode, ident : String, tipe : Type)

    /**
     * A built-in procedure with its parameter information.
     */
    case class BuiltinProc (ident : String, params : Seq[ParamInfo]) extends NamedEntity with Builtin

    /**
     * The built-in Read procedure.
     */
    lazy val readProc =
        BuiltinProc ("Read", Seq (ParamInfo (VarMode (), "ReadParam", integerType)))

    /**
     * The built-in Write procedure.
     */
    lazy val writeProc =
        BuiltinProc ("Write", Seq (ParamInfo (ValMode (), "WriteParam", integerType)))

    /**
     * The built-in WriteLn procedure.
     */
    lazy val writelnProc =
        BuiltinProc ("WriteLn", Nil)

    /**
     * The default environment with pre-defined procedures added.
     */
    override def defenvPairs : Seq[(String,Entity)] =
        Seq (
            "Read"    -> readProc,
            "Write"   -> writeProc,
            "WriteLn" -> writelnProc
        ) ++
        super.defenvPairs

}
