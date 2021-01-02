/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2021 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2021 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.obr

import org.bitbucket.inkytonik.kiama.util.{Entity, Environments}

/**
 * Superclass of all Obr entities.
 */
sealed abstract class ObrEntity extends Entity with Product

/**
 * Symbol table module containing facilities for creating and
 * manipulating Obr language symbol information.
 */
object SymbolTable extends Environments[ObrEntity] {

    import ObrTree._

    /**
     * A variable entity of the given type.
     */
    case class Variable(tipe : Type) extends ObrEntity

    /**
     * A constant integer entity with the given type and value.
     * Can represent an integer or an enumeration constant.
     */
    case class Constant(tipe : Type, value : Int) extends ObrEntity

    /**
     * An entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case class MultipleEntity() extends ObrEntity {
        override val isError = true
    }

    /**
     * An unknown entity, for example one that is represened by names whose
     * declarations are missing.
     */
    case class UnknownEntity() extends ObrEntity {
        override val isError = true
    }

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
        def iscompatible(other : TypeBase) : Boolean =
            (other == UnknownType()) || (other == this)
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
    case class IntType() extends Type {
        override val storage = WORDSIZE
        override def toString : String = "integer"
    }

    /**
     * The Boolean type.
     */
    case class BoolType() extends Type {
        override val storage = WORDSIZE
        override def toString : String = "boolean"
    }

    /**
     * An integer array type.
     */
    case class ArrayType(size : Int) extends Type {
        override val storage = WORDSIZE * size
        override def toString : String = "array"
    }

    /**
     * A record type with the given fields.
     */
    case class RecordType(fields : Vector[Identifier]) extends Type {
        override val storage = WORDSIZE * fields.length
        override def toString : String = "record"
    }

    /**
     * The following is not an actual type, but it is compatible with
     * every record type - and so is useful when type checking constructs
     * which must take a value of an arbitrary record type.
     */
    case class RecordTypes() extends TypeBase {
        override def toString : String = "any record"
        override def iscompatible(other : TypeBase) : Boolean =
            (other.isInstanceOf[RecordType]) || (super.iscompatible(other))
    }

    /**
     * A named enumeration type.
     */
    case class EnumType(ident : Identifier) extends Type {
        override val storage = WORDSIZE
        override def toString : String = s"enumeration $ident"
    }

    /**
     * The following is not an actual type, but it is compatible with
     * every enumeration type - and so is useful when type checking constructs
     * which must take a value of an arbitrary enumeration type.
     */
    case class EnumTypes() extends TypeBase {
        override def toString : String = "any enumeration"
        override def iscompatible(other : TypeBase) : Boolean =
            (other.isInstanceOf[EnumType]) || (super.iscompatible(other))
    }

    /**
     * The exception type.
     */
    case class ExnType() extends Type {
        override val storage = WORDSIZE
        override def toString : String = "exception"
    }

    /**
     * A type that we don't know anything about.
     */
    case class UnknownType() extends Type {
        override def iscompatible(other : TypeBase) : Boolean = true
    }

}
