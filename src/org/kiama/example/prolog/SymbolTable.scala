/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package example.prolog

import org.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments {

    import PrologTree._
    import scala.collection.immutable.Map

    /**
     * A predicate entity and its argument type constraints.
     */
    case class Predicate (argtypes : List[Type]) extends Entity

    /**
     * A variable entity including the type constraint that we know so far.
     */
    case class Variable (tipe : Type) extends Entity

    /** 
     * The type of a predicate argument.
     */
    abstract class Type

    /**
     * The atom type.
     */
    case object AtomType extends Type {
        override def toString = "atom"
    }

    /**
     * The integer type.
     */
    case object IntegerType extends Type {
        override def toString = "integer"
    }

    /**
     * The list type.
     */
    case object ListType extends Type {
        override def toString = "list"
    }

    /**
     * A type that is unknown.
     */
    case object UnknownType extends Type
    
}
