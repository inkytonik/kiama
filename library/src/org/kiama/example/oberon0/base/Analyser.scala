/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package example.oberon0
package base

trait Analyser extends SymbolTable {

    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators
    import org.kiama.rewriting.Rewriter.collectall
    import org.kiama.util.Messaging.{Messages, noMessages}
    import source.SourceNode
    import source.SourceTree.SourceTree

    /**
     * The tree in which this analysis is being performed.
     */
    def tree : SourceTree

    /**
     * Decorators on the analysed tree.
     */
    lazy val decorators = new Decorators (tree)

    /**
     * The semantic errors for a tree.
     */
    val errors =
        attr (collectall {
            case n : SourceNode =>
                errorsDef (n)
        })

    /**
     * The error checking for this level, overridden to extend at later
     * levels. No errors are collected at this level.
     */
    def errorsDef (n : SourceNode) : Messages =
        noMessages

}
