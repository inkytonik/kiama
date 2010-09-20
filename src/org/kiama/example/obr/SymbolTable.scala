/**
 * Symbol tables for the Obr language.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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

package org.kiama.example.obr

/**
 * Symbol table module containing facilities for creating and
 * manipulating Obr language symbol information.
 */
object SymbolTable {

    import ObrTree._
    import scala.collection.immutable.Map

    /**
     * An environment is a map from identifiers to entities.  I.e.
     * the bindings are the entries in the map.
     */
    type Environment = Map[Identifier,Entity]

    /**
     * The most recent memory location allocated to a variable, or
     * alternatively the size of memory minus one.
     */
    var prevloc = 0

    /**
     * Reset the symbol table.
     */
    def reset () {
        prevloc = 0
    }

    /**
     * An entity represents a thing that an expression language program
     * can create and act on.
     */
    abstract class Entity {
        val isconst = false
        val isassignable = true
        val tipe : Type = UnknownType
        val locn = 0
    }

    /**
     * A variable entity of the given type.  Allocation of a location
     * for the variable assumes that all variables take four bytes.
     */
    case class Variable (override val tipe : Type) extends Entity {
        override val isconst = false
        override val isassignable =
            (tipe == IntType) || (tipe == BoolType) || (tipe.isInstanceOf[EnumType])
        override val locn = {
            val loc = prevloc
            prevloc = prevloc + tipe.storage
            loc
        }
    }

    /**
     * A constant integer entity with the given value.
     * Can represent an integer or an enumeration constant.
     */
    case class Constant (override val tipe : Type, val value : Int) extends Entity {
        override val isconst = true
        override val isassignable = false
    }

    /**
     * A singleton entity about which we know nothing.  Used as the
     * entity referred to by names that aren't declared.
     */
    case object Unknown extends Entity

    /**
     * A singleton entity about which we know too much.  Used as the
     * entity referred to by names that are declared more than once
     * in the same scope.
     */
    case object Multiple extends Entity

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
            (other == UnknownType) || (other == this)
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
    case object IntType extends Type {
        override val storage = WORDSIZE
        override def toString = "integer"
    }

    /**
     * The Boolean type.
     */
    case object BoolType extends Type {
        override val storage = WORDSIZE
        override def toString = "boolean"
    }

    /**
     * An integer array type.
     */
    case class ArrayType (size : Int) extends Type {
        override val storage = WORDSIZE * size
        override def toString = "array"
    }

    /**
     * A record type with the given fields.
     */
    case class RecordType (fields : List[Identifier]) extends Type {
        override val storage = WORDSIZE * fields.length
        override def toString = "record"
    }

    /**
     * The following is not an actual type, but it is compatible with
     * every record type - and so is useful when type checking constructs
     * which must take a value of an arbitrary record type.
     */
    case object RecordTypes extends TypeBase {
        override def toString = "any record"
        override def iscompatible (other : TypeBase) : Boolean =
            (other.isInstanceOf[RecordType]) || (super.iscompatible (other))
    }

    /**
     * A named enumeration type.
     */
    case class EnumType (ident : Identifier) extends Type {
        override val storage = WORDSIZE
        override def toString = "enumeration " + ident
    }

    /**
     * The following is not an actual type, but it is compatible with
     * every enumeration type - and so is useful when type checking constructs
     * which must take a value of an arbitrary enumeration type.
     */
    case object EnumTypes extends TypeBase {
        override def toString = "any enumeration"
        override def iscompatible (other : TypeBase) : Boolean =
            (other.isInstanceOf[EnumType]) || (super.iscompatible (other))
    }

    /**
     * A type that we don't know anything about.
     */
    case object UnknownType extends Type {
        override def iscompatible (other : TypeBase) : Boolean = true
    }

}
