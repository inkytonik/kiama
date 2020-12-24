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

import org.bitbucket.inkytonik.kiama.attribution.Attribution

trait Analyser extends Attribution with SymbolTable {

    import org.bitbucket.inkytonik.kiama.attribution.Decorators
    import org.bitbucket.inkytonik.kiama.util.Messaging.{collectMessages, Messages, noMessages}
    import source.SourceNode
    import source.SourceTree.SourceTree

    /**
     * The tree in which this analysis is being performed.
     */
    def tree : SourceTree

    /**
     * Decorators on the analysed tree.
     */
    final lazy val decorators = new Decorators(tree)

    /**
     * The semantic errors for the tree.
     */
    lazy val errors : Messages =
        collectMessages(tree) {
            case n =>
                errorsDef(n)
        }

    /**
     * The error checking for this level, overridden to extend at later
     * levels. No errors are collected at this level.
     */
    def errorsDef(n : SourceNode) : Messages =
        noMessages

}
