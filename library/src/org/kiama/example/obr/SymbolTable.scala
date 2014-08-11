/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2014 Dominic Verity, Macquarie University.
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
package example.obr

import org.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating Obr language symbol information.
 */
object SymbolTable extends Environments {

    import ObrTree._
    import scala.collection.immutable.Seq
    import org.kiama.util.Entity

    /**
     * A variable entity of the given type.
     */
    case class Variable (tipe : Type) extends Entity

    /**
     * A constant integer entity with the given type and value.
     * Can represent an integer or an enumeration constant.
     */
    case class Constant (tipe : Type, value : Int) extends Entity

    /**
     * The size in bytes of a word used to store both integer and Boolean
     * values.
     */
    val WORDSIZE = 4

    /**
     * Superclasses of all type representations.
     *
     * TypeBase provides a method which is used to determine if two types
     * are compatible. If either type is unknown then we assume an error
     * has already been raised elsewhere so we say that such an unknown type
     * is compatible with everything.
     */
    abstract class TypeBase {
        def iscompatible (other : TypeBase) : Boolean =
            (other == UnknownType ()) || (other == this)
    }

    /**
     * All genuine types know how much storage in bytes is needed to store a
     * value of their type.
     */
    abstract class Type extends TypeBase {
        val storage = 0
    }

    /**
     * The integer type.
     */
    case class IntType () extends Type {
        override val storage = WORDSIZE
        override def toString : String = "integer"
    }

    /**
     * The Boolean type.
     */
    case class BoolType () extends Type {
        override val storage = WORDSIZE
        override def toString : String = "boolean"
    }

    /**
     * An integer array type.
     */
    case class ArrayType (size : Int) extends Type {
        override val storage = WORDSIZE * size
        override def toString : String = "array"
    }

    /**
     * A record type with the given fields.
     */
    case class RecordType (fields : Seq[Identifier]) extends Type {
        override val storage = WORDSIZE * fields.length
        override def toString : String = "record"
    }

    /**
     * The following is not an actual type, but it is compatible with
     * every record type - and so is useful when type checking constructs
     * which must take a value of an arbitrary record type.
     */
    case class RecordTypes () extends TypeBase {
        override def toString : String = "any record"
        override def iscompatible (other : TypeBase) : Boolean =
            (other.isInstanceOf[RecordType]) || (super.iscompatible (other))
    }

    /**
     * A named enumeration type.
     */
    case class EnumType (ident : Identifier) extends Type {
        override val storage = WORDSIZE
        override def toString : String = s"enumeration $ident"
    }

    /**
     * The following is not an actual type, but it is compatible with
     * every enumeration type - and so is useful when type checking constructs
     * which must take a value of an arbitrary enumeration type.
     */
    case class EnumTypes () extends TypeBase {
        override def toString : String = "any enumeration"
        override def iscompatible (other : TypeBase) : Boolean =
            (other.isInstanceOf[EnumType]) || (super.iscompatible (other))
    }

    /**
     * The exception type.
     */
    case class ExnType () extends Type {
        override val storage = WORDSIZE
        override def toString : String = "exception"
    }


    /**
     * A type that we don't know anything about.
     */
    case class UnknownType () extends Type {
        override def iscompatible (other : TypeBase) : Boolean = true
    }

}
