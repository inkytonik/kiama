/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.til

/**
 * Move all declarations to the start of the program.
 */
class TIL2_3 extends TransformingMain {

    import TILTree._
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.util.{Config, Source}

    def parse(source : Source, config : Config) : ParseResult[Program] = {
        val parsers = new TIL1_1Parsers(positions)
        parsers.parseAll(parsers.program, source)
    }

    def transform(ast : Program) : Program = {
        val decls = Vector.newBuilder[Decl]
        val getandremovedecls =
            everywhere(rule[List[Stat]] {
                case (d : Decl) :: ss =>
                    decls += d
                    ss
            })
        val Program(stmts) = rewrite(getandremovedecls)(ast)
        Program(decls.result.toList ++ stmts)
    }

}

object TIL2_3Main extends TIL2_3
