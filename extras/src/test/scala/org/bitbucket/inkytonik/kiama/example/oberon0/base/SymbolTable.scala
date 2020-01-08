/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package base

import org.bitbucket.inkytonik.kiama.util.{Entity, Environments}

/**
 * Superclass of all Oberon0 entities.
 */
abstract class Oberon0Entity extends Entity with Product

/**
 * Basic symbol table definitions.
 */
trait SymbolTable extends Environments[Oberon0Entity] {

    /**
     * An entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case class MultipleEntity() extends Oberon0Entity {
        override val isError = true
    }

    /**
     * An unknown entity, for example one that is represened by names whose
     * declarations are missing.
     */
    case class UnknownEntity() extends Oberon0Entity {
        override val isError = true
    }

}
