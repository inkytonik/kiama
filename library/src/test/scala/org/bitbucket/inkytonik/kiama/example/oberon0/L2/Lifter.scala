/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L2

/**
 * Lifting transformation for L2.
 */
trait Lifter extends base.Transformer {

    import base.source.{Block, Declaration, ModuleDecl}
    import base.source.SourceTree.SourceTree
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rewriteTree, rule}

    /**
     * Lift inner declarations within the module to the top level.  Assumes
     * that identifiers are unique. Then call the next level of transformation.
     */
    override def transform(tree : SourceTree) : SourceTree = {

        /*
         * The collected declarations.
         */
        val decls = Vector.newBuilder[Declaration]

        /*
         * Lift declarations from inner blocks to the top level by adding
         * them to the declarations buffer in a bottom-up fashion and
         * removing them from their blocks.
         */
        lazy val liftBlocks =
            everywherebu(
                rule[Block] {

                    // Add this block's decls to the buffer, clear them
                    case Block(ds, ss) =>
                        decls ++= ds
                        Block(Vector(), ss)

                } <+ rule[ModuleDecl] {

                    // The module declarations will have been added to the
                    // buffer already. Create a new module with all of the
                    // accumulated declarations.
                    case ModuleDecl(i1, Block(Vector(), ss), i2) =>
                        ModuleDecl(i1, Block(decls.result, ss), i2)

                }
            )

        super.transform(rewriteTree(liftBlocks)(tree))

    }

}
