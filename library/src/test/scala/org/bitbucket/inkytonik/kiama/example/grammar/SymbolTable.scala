/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.grammar

import org.bitbucket.inkytonik.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating grammar symbol information.
 */
object SymbolTable extends Environments {

    import GrammarTree._
    import org.bitbucket.inkytonik.kiama.util.Entity

    /**
     * A non-terminal entity containing a reference to the production that
     * defines the non-terminal.
     */
    case class NonTerminal(rule : Rule) extends Entity

}
