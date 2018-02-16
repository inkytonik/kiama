/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.til

/**
 * Rewrite TILs for loops that automatically declare the control variable
 * adding an explicit declaration of the variable.
 */
class TIL2_1 extends TransformingMain {

    import TILTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

    val parsers = new TIL1_1Parsers(positions)
    val parser = parsers.program

    def transform(ast : Program) : Program =
        rewrite(declareforvars)(ast)

    val declareforvars =
        everywherebu(rule[List[Stat]] {
            case (s @ For(Id(i), f, t, b)) :: ss =>
                Decl(Id(i)) :: s :: ss
        })

}

object TIL2_1Main extends TIL2_1
