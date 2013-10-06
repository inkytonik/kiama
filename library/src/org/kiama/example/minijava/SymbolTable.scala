/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2013 Anthony M Sloane, Macquarie University.
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
package example.minijava

import org.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating MiniJava language symbol information.
 */
object SymbolTable extends Environments {

    import MiniJavaTree._

    /**
     * A main class entity (i.e., the one that is used to start a program).
     * The `decl` field gives us access back to the declaration from the
     * entity.
     */
    case class MainClassEntity (decl : MainClass) extends Entity

    /**
     * A normal class entity (i.e., all the non-main classes). The `decl`
     * field gives us access back to the declaration from the entity.
     */
    case class ClassEntity (decl : Class) extends Entity

    /**
     * A method entity. The `decl` field gives us access back to the
     * declaration from the entity.
     */
    case class MethodEntity (decl : Method) extends Entity

    /**
     * An entity representing an argument to a method. The `decl` field
     * gives us access back to the declaration from the entity.
     */
    case class ArgumentEntity (decl : Argument) extends Entity

    /**
     * A instance variable (field) entity. The `decl` field gives us access
     * back to the declaration from the entity.
     */
    case class FieldEntity (decl : Field) extends Entity

    /**
     * A local variable entity. The `decl` field gives us access back
     * to the declaration from the entity.
     */
    case class VariableEntity (decl : Var) extends Entity

    // Internal types, not created from user programs by the parser but
    // used to represent some types internally to the semantic analysis.

    /**
     * A reference type given by the declared class body.
     */
    case class ReferenceType (decl : Class) extends Type

    /**
     * An unknown type, for example, one belonging to a name that is not declared
     * but is used in an expression.
     */
    case class UnknownType () extends Type

}
