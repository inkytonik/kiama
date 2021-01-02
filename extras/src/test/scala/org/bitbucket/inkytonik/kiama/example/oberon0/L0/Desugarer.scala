/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L0

/**
 * Desugaring transformation for L0.
 */
trait Desugarer extends base.Transformer {

    import base.source.{Identifier, IdnDef, IdnUse}
    import base.source.SourceTree.SourceTree
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rewriteTree, rule}

    /**
     * Desugar the provided module to replace identifier uses with uses
     * of unique names. Then call the next level of transformation.
     */
    override def transform(tree : SourceTree) : SourceTree =
        super.transform(uniquifyNames(tree))

    /**
     * A way to build other instances of the relevant analysis stack. Used by
     * the transformer to apply name analysis to trees that may not be the
     * original tree.
     */
    def buildAnalyser(atree : SourceTree) : TypeAnalyser

    /**
     * Rename user-defined names to avoid clashes with outer declarations
     * of the same name.  This transformation is not idempotent.
     */
    def uniquifyNames(t : SourceTree) : SourceTree = {

        /*
         * An analyser for the input tree.
         */
        val analyser = buildAnalyser(t)
        import analyser.{entity, isBuiltin, Named}

        /*
         * The name to use for a particular name occurrence.  If the occurrence
         * denotes a named entity, use that entity's id, otherwise leave the
         * occurrence unchanged.
         */
        def nameOf(i : Identifier, isdef : Boolean) : Identifier =
            entity(i) match {
                case e : Named if isdef =>
                    IdnDef(e.id)
                case e : Named =>
                    IdnUse(e.id)
                case _ =>
                    i
            }

        /*
         * Rename any user-defined name to its id if it has one.
         */
        val renameNames =
            everywherebu(
                rule[Identifier] {
                    case i : IdnDef =>
                        nameOf(i, true)
                    case i @ IdnUse(s) =>
                        if (isBuiltin(entity(i)))
                            i
                        else
                            nameOf(i, false)
                }
            )

        rewriteTree(renameNames)(t)

    }

}
