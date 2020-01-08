/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.til

/**
 * Transform for loops into equivalent while loops.
 */
class TIL2_2 extends TransformingMain {

    import TILTree._
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.util.Source

    def parse(source : Source) : ParseResult[Program] = {
        val parsers = new TIL1_1Parsers(positions)
        parsers.parseAll(parsers.program, source)
    }

    def transform(ast : Program) : Program =
        rewrite(fortowhile)(ast)

    val fortowhile =
        everywhere(rule[List[Stat]] {
            case (s @ For(id @ Id(i), f, t, b)) :: ss =>
                val upperid = Id(s"Upper$i")
                Decl(id) ::
                    Assign(id, f) ::
                    Decl(upperid) ::
                    Assign(upperid, Add(t, Num(1))) ::
                    While(
                        Sub(Var(id), Var(upperid)),
                        b ++ List(Assign(id, Add(Var(id), Num(1))))
                    ) ::
                        ss
        })

}

object TIL2_2Main extends TIL2_2
