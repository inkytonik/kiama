/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L3

trait SymbolTable extends L0.SymbolTable {

    import org.bitbucket.inkytonik.kiama.util.Entity
    import source.{Mode, ProcDecl, ValMode, VarMode}

    /**
     * A procedure entity represented by a procedure declaration.
     */
    case class Procedure(ident : String, decl : ProcDecl) extends NamedEntity

    /**
     * A parameter is a variable augmented with a passing mode.
     */
    case class Parameter(mode : Mode, varr : Variable) extends NamedEntity {
        def ident : String = varr.ident
    }

    /**
     * Parameters are variables too.
     */
    override def isVariable(e : Entity) : Boolean =
        super.isVariable(e) || e.isInstanceOf[Parameter]

    /**
     * Information about a particular parameter.  Similar to Parameter but the type
     * has been replaced with its definition.
     */
    case class ParamInfo(mode : Mode, ident : String, tipe : Type)

    /**
     * A built-in procedure with its parameter information.
     */
    case class BuiltinProc(ident : String, params : Vector[ParamInfo]) extends Entity

    /**
     * The built-in Read procedure.
     */
    lazy val readProc =
        BuiltinProc("Read", Vector(ParamInfo(VarMode(), "ReadParam", integerType)))

    /**
     * The built-in Write procedure.
     */
    lazy val writeProc =
        BuiltinProc("Write", Vector(ParamInfo(ValMode(), "WriteParam", integerType)))

    /**
     * The built-in WriteLn procedure.
     */
    lazy val writelnProc =
        BuiltinProc("WriteLn", Vector())

    /**
     * Return true if the entity is a builtin, false otherwise.
     */
    override def isBuiltin(e : Entity) : Boolean =
        super.isBuiltin(e) || e.isInstanceOf[BuiltinProc]

    /**
     * The default environment with pre-defined procedures added.
     */
    override def defenvPairs : List[(String, Entity)] =
        List(
            "Read" -> readProc,
            "Write" -> writeProc,
            "WriteLn" -> writelnProc
        ) ++
            super.defenvPairs

}
