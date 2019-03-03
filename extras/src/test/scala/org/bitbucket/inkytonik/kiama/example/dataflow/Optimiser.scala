/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.dataflow

import DataflowTree.DataflowTree
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

/**
 * Optimise a dataflow program.  Currently: a) eliminate assignments to
 * variables that are not live out of the assignment and b) remove empty
 * statements from sequences.
 */
class Optimiser(override val tree : DataflowTree) extends Dataflow(tree) {

    import DataflowTree._

    def run(t : Stm) : Stm =
        rewrite(rules)(t)

    lazy val rules = elimDeadAssign <* elimEmpties

    lazy val elimDeadAssign =
        alltd(rule[Stm] {
            case s @ Assign(v, _) if !(out(s) contains v) =>
                Empty()
        })

    lazy val elimEmpties =
        bottomup(attempt(rule[List[Stm]] {
            case Empty() :: ss => ss
        } <+ rule[Stm] {
            case Block(Nil) => Empty()
        }))

}
