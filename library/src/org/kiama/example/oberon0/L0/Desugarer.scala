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
package L0

/**
 * Desugaring transformation for L0.
 */
trait Desugarer extends base.Transformer with NameAnalyser {

    import base.source.{Identifier, IdnDef, IdnUse, ModuleDecl}
    import org.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}
    import org.kiama.attribution.Attribution.initTree

    /**
     * Desugar the provided module to replace identifier uses with uses
     * of unique names. Then call the next level of transformation.
     */
    override def transform (m : ModuleDecl) : ModuleDecl = {
        initTree (m)
        super.transform (uniquifyNames (m))
    }

    /**
     * Rename user-defined names to avoid clashes with outer declarations
     * of the same name.  This transformation is not idempotent.
     */
    def uniquifyNames (m : ModuleDecl) : ModuleDecl = {

        /**
         * The name to use for a particular name occurrence.  If the occurrence
         * denotes a named entity, use that entity's id, otherwise leave the
         * occurrence unchanged.
         */
        def nameOf (i : Identifier, isdef : Boolean) : Identifier =
            (i->entity) match {
                case e : Named => if (isdef) IdnDef (e.id) else IdnUse (e.id)
                case _         => i
            }

        /**
         * Rename any user-defined name that is used more than once to its id
         * if it has one.
         */
        val renameNames =
            everywherebu (
                rule[Identifier] {
                    case i : IdnDef => nameOf (i, true)
                    case i @ IdnUse (s) =>
                        (i->entity) match {
                            case b : Builtin => i
                            case _           => nameOf (i, false)
                        }
                }
            )

        rewrite (renameNames) (m)

    }

}
