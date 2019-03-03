/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.minijava

import MiniJavaTree._
import org.bitbucket.inkytonik.kiama.util.{Entity, Environments}

/**
 * Superclass of all MiniJava entities. Provides generic access to
 * the declaration node of the entity and a textual description.
 */
sealed abstract class MiniJavaEntity extends Entity with Product

/**
 * Symbol table module containing facilities for creating and
 * manipulating MiniJava language symbol information.
 */
object SymbolTable extends Environments[MiniJavaEntity] {

    /**
     * A MiniJava entity that represents a legally resolved entity.
     */
    sealed abstract class MiniJavaOkEntity extends MiniJavaEntity {
        def decl : MiniJavaNode
        def desc : String
    }

    /**
     * A main class entity (i.e., the one that is used to start a program).
     * The `decl` field gives us access back to the declaration from the
     * entity.
     */
    case class MainClassEntity(decl : MainClass) extends MiniJavaOkEntity {
        val desc = "main class"
    }

    /**
     * A normal class entity (i.e., all the non-main classes). The `decl`
     * field gives us access back to the declaration from the entity.
     */
    case class ClassEntity(decl : Class) extends MiniJavaOkEntity {
        val desc = "class"
    }

    /**
     * A method entity. The `decl` field gives us access back to the
     * declaration from the entity.
     */
    case class MethodEntity(decl : Method) extends MiniJavaOkEntity {
        val desc = "method"
    }

    /**
     * An entity representing an argument to a method. The `decl` field
     * gives us access back to the declaration from the entity.
     */
    case class ArgumentEntity(decl : Argument) extends MiniJavaOkEntity {
        val desc = "method argument"
    }

    /**
     * A instance variable (field) entity. The `decl` field gives us access
     * back to the declaration from the entity.
     */
    case class FieldEntity(decl : Field) extends MiniJavaOkEntity {
        val desc = "class field"
    }

    /**
     * A local variable entity. The `decl` field gives us access back
     * to the declaration from the entity.
     */
    case class VariableEntity(decl : Var) extends MiniJavaOkEntity {
        val desc = "local variable"
    }

    /**
     * An entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case class MultipleEntity() extends MiniJavaEntity {
        override val isError = true
    }

    /**
     * An unknown entity, for example one that is represened by names whose
     * declarations are missing.
     */
    case class UnknownEntity() extends MiniJavaEntity {
        override val isError = true
    }

    // Internal types, not created from user programs by the parser but
    // used to represent some types internally to the semantic analysis.

    /**
     * A reference type given by the declared class body.
     */
    case class ReferenceType(decl : Class) extends Type

    /**
     * An unknown type, for example, one belonging to a name that is not declared
     * but is used in an expression.
     */
    case class UnknownType() extends Type

}
