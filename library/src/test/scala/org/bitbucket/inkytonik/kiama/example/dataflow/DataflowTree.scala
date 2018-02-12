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
package example.dataflow

/**
 * Imperative language AST for dataflow example.
 */
object DataflowTree {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    type DataflowTree = Tree[Stm, Stm]

    type Var = String

    abstract class Stm extends Product

    case class Assign(left : Var, right : Var) extends Stm
    case class While(cond : Var, body : Stm) extends Stm
    case class If(cond : Var, tru : Stm, fls : Stm) extends Stm
    case class Block(stms : List[Stm]) extends Stm
    case class Return(ret : Var) extends Stm
    case class Empty() extends Stm

}
