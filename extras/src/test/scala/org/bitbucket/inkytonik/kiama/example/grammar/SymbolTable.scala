/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.grammar

import org.bitbucket.inkytonik.kiama.util.{Entity, Environments}

/**
 * Superclass of all grammar entities.
 */
sealed abstract class GrammarEntity extends Entity with Product

/**
 * Symbol table module containing facilities for creating and
 * manipulating grammar symbol information.
 */
object SymbolTable extends Environments[GrammarEntity] {

    import GrammarTree._

    /**
     * A non-terminal entity containing a reference to the production that
     * defines the non-terminal.
     */
    case class NonTerminal(rule : Rule) extends GrammarEntity

    /**
     * An entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case class MultipleEntity() extends GrammarEntity {
        val desc = "multiply-defined"
    }

    /**
     * An unknown entity, for example one that is represened by names whose
     * declarations are missing.
     */
    case class UnknownEntity() extends GrammarEntity {
        val desc = "unknown"
    }

}
