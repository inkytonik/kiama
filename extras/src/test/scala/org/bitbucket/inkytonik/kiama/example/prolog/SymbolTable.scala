/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.prolog

import org.bitbucket.inkytonik.kiama.util.{Entity, Environments}

/**
 * Superclass of all Obr entities.
 */
sealed abstract class PrologEntity extends Entity with Product

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments[PrologEntity] {

    /**
     * A predicate entity and its argument type constraints.
     */
    case class Predicate(argtypes : Vector[Type]) extends PrologEntity

    /**
     * A variable entity including the type constraint that we know so far.
     */
    case class Variable(tipe : Type) extends PrologEntity

    /**
     * An entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case class MultipleEntity() extends PrologEntity {
        override val isError = true
    }

    /**
     * An unknown entity, for example one that is represened by names whose
     * declarations are missing.
     */
    case class UnknownEntity() extends PrologEntity {
        override val isError = true
    }

    /**
     * The type of a predicate argument.
     */
    abstract class Type

    /**
     * The atom type.
     */
    case class AtomType() extends Type {
        override def toString : String = "atom"
    }

    /**
     * The integer type.
     */
    case class IntegerType() extends Type {
        override def toString : String = "integer"
    }

    /**
     * The list type.
     */
    case class ListType() extends Type {
        override def toString : String = "list"
    }

    /**
     * A type that is unknown.
     */
    case class UnknownType() extends Type

}
