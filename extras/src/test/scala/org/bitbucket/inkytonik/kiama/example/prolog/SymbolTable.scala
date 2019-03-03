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

import org.bitbucket.inkytonik.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments {

    import org.bitbucket.inkytonik.kiama.util.Entity

    /**
     * A predicate entity and its argument type constraints.
     */
    case class Predicate(argtypes : Vector[Type]) extends Entity

    /**
     * A variable entity including the type constraint that we know so far.
     */
    case class Variable(tipe : Type) extends Entity

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
