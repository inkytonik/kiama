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
package example.dataflow

import DataflowTree._

case class Foreach(cond : Var, body : Stm) extends Stm

case class For(init : Stm, c : Stm, inc : Stm, body : Stm) extends Stm

class DataflowFor(override val tree : DataflowTree) extends Dataflow(tree) {

    def addForAndForeachCases() {

        succ +=
            {
                case t @ Foreach(_, body) =>
                    following(t) + body
            }

        following +=
            {
                case tree.parent(parent : Foreach) =>
                    following(parent) + parent.body
            }

        succ +=
            {
                case For(init, c, inc, body) =>
                    Set(init)
            }

        following +=
            {
                case s @ tree.parent(parent : For) =>
                    parent match {
                        case t @ For(s1, c, _, _) if s eq s1 => Set(c)
                        case t @ For(_, s1, _, b) if s eq s1 => following(t) + b
                        case t @ For(_, c, s1, _) if s eq s1 => Set(c)
                        case t @ For(_, _, i, s1) if s eq s1 => Set(i)
                    }
            }

    }

}
