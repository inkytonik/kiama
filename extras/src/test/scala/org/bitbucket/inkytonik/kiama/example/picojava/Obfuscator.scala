/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2021 Matthew Roberts, Macquarie University.
 * Copyright (C) 2013-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.picojava

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter

/**
 * Transform a program into an equivalent obsfuscated program.
 */
class Obfuscator(analysis : NameResolution) extends Rewriter {

    import PicoJavaTree._

    /**
     * Choose between which traversal approach to use.
     */
    val optimiseTraversal = true

    /**
     * Obfuscate a program by renaming variable and class identifiers.
     * The variables and classes are numbered and the input names replaced
     * by `v` or `c` followed by a number.
     */
    def obfuscate(p : Program) : Program = {

        import org.bitbucket.inkytonik.kiama.util.Counter

        // Map from declaration nodes to new variable names
        val declNames = scala.collection.mutable.Map[Decl, String]()

        // Counter to generate unique names
        val uniqueNameCounter = new Counter

        /*
         * Make and return a new name for declaration `d` and remember it in
         * the map.
         */
        def makeName(d : Decl) : String = {
            val count = uniqueNameCounter.next()
            val varname = s"n$count"
            declNames += ((d, varname))
            varname
        }

        /*
         * Replace a variable or class declaration with a copy using a
         * generated name.
         */
        val obfuscateDecl =
            rule[Decl] {
                case d : VarDecl =>
                    d.copy(Name = makeName(d))
                case d : ClassDecl =>
                    d.copy(Name = makeName(d))
            }

        /*
         * Obfuscate all of the variable and class declarations in a program.
         */
        val obfuscateDecls =
            topdown(attempt(obfuscateDecl))

        /*
         * Sequence of names that we do not want to replace.
         */
        val predefinedNames = List("boolean", "int")

        /*
         * Rule that detects pre-defined identifiers and leaves them unchanged
         */
        val preservePredefinedUse =
            rule[Use] {
                case u @ Use(name) if predefinedNames contains name =>
                    u
            }

        /*
         * Version of `preservePredefinedUse` that reduces number of nodes
         * that `obfuscateNormalUse` has to consider by reversing the test.
         * In other words, this one succeeds if `obfuscateNormalUse` should
         * process the node. See also `obfuscateUses2` below.
         */
        val preservePredefinedUse2 =
            rule[Use] {
                case u @ Use(name) if !(predefinedNames contains name) =>
                    u
            }

        /*
         * Rule that replaces an identifier use with the new name which was
         * determined for that identifier's declaration.
         */
        val obfuscateNormalUse =
            rule[Use] {
                case u =>
                    u.copy(Name = declNames.getOrElse(analysis.decl(u), "$UNDEF$"))
            }

        /*
         * Obfuscate all identifier uses in the program.
         */
        val obfuscateUses =
            topdown(attempt(preservePredefinedUse <+ obfuscateNormalUse))

        /*
         * Version of `obfuscateUses` that uses `preservePredefinedUse2` and
         * only moves to `obfuscateNormalUse` if the former suceeds.
         */
        val obfuscateUses2 =
            topdown(attempt(preservePredefinedUse2 <* obfuscateNormalUse))

        /*
         * Combined strategy to obfuscate a program.
         */
        val obfuscateProgram =
            (obfuscateDecls <* obfuscateUses)

        /*
         * Version of `obfuscateProgram` that uses `obfuscateUses2`.
         */
        val obfuscateProgram2 =
            (obfuscateDecls <* obfuscateUses2)

        // Actually make the transformation

        if (optimiseTraversal)
            rewrite(obfuscateProgram2)(p)
        else
            rewrite(obfuscateProgram)(p)

    }

}
