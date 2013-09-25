/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Matthew Roberts, Macquarie University.
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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
package example.picojava

import org.kiama.rewriting.Rewriter

/**
 * Transform a program into an equivalent obsfuscated program.
 */
object Obfuscate extends Rewriter {

    import AbstractSyntax._
    import NameResolution._
    import attribution.Attribution._
    import scala.collection.mutable.Map

    /**
     * Obfuscate a program by renaming variable and class identifiers.
     * The variables and classes are numbered and the input names replaced
     * by `v` or `c` followed by a number.
     */
    def obfuscate (p : Program) : Program = {

        // Map from declaration nodes to new variable names
        val declNames = Map[Decl, String] ()

        // The number of the most recent name that was used
        var next : Int = -1

        /**
         * Make and return a new name for declaration `d` and remember it in
         * the map.
         */
        def makeName (d : Decl) : String = {
            next = next + 1
            val varname = s"n$next"
            declNames += ((d, varname))
            varname
        }

        /**
         * Replace a variable or class declaration with a copy using a
         * generated name.
         */
        val obfuscateDecl =
            rule {
                case d : VarDecl =>
                    d.copy (Name = makeName (d))
                case d : ClassDecl =>
                    d.copy (Name = makeName (d))
            }

        /**
         * Obfuscate all of the variable and class declarations in a program.
         */
        val obfuscateDecls =
            topdown (attempt (obfuscateDecl))

        /**
         * List of names that we do not want to replace.
         */
        val predefinedNames = List ("boolean", "int")

        /**
         * Rule that detects pre-defined identifiers and leaves them unchanged
         */
        val preservePredefinedUse =
            rule {
                case u @ Use (name) if predefinedNames contains name =>
                    u
            }

        /**
         * Version of `preservePredefinedUse` that reduces number of nodes
         * that `obfuscateNormalUse` has to consider by reversing the test.
         * In other words, this one succeeds if `obfuscateNormalUse` should
         * process the node. See also `obfuscateUses2` below.
         */
        val preservePredefinedUse2 =
            rule {
                case u @ Use (name) if ! (predefinedNames contains name) =>
                    u
            }

        /**
         * Rule that replaces an identifier use with the new name which was
         * determined for that identifier's declaration.
         */
        val obfuscateNormalUse =
            rule {
                case u : Use =>
                    u.copy (Name = declNames.getOrElse (u->decl, "$UNDEF$"))
            }

        /**
         * Obfuscate all identifier uses in the program.
         */
        val obfuscateUses =
            topdown (attempt (preservePredefinedUse <+ obfuscateNormalUse))

        /**
         * Version of `obfuscateUses` that uses `preservePredefinedUse2` and
         * only moves to `obfuscateNormalUse` if the former suceeds.
         */
        val obfuscateUses2 =
            topdown (attempt (preservePredefinedUse2 <* obfuscateNormalUse))

        /**
         * Combined strategy to obfuscate a program.
         */
        val obfuscateProgram =
            (obfuscateDecls <* obfuscateUses)

        /**
         * Version of `obfuscateProgram` that uses `obfuscateUses2`.
         */
        val obfuscateProgram2 =
            (obfuscateDecls <* obfuscateUses2)

        // Actually make the transformation

        initTree (p)
        rewrite (obfuscateProgram) (p)
        // rewrite (obfuscateProgram2) (p)

    }

}
