/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.transform

/**
 * Abstract syntax.
 */
object TransformTree {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    /**
     * Tree type for transform trees.
     */
    type TransformTree = Tree[TransformNode, Program]

    /**
     * Abstract syntax for transform tree nodes.
     */
    sealed abstract class TransformNode extends Product

    /**
     * Nodes that have entities associated with them.
     */
    sealed trait EntityNode extends TransformNode

    /**
     * A program is a map from operator names to priorities, a right recursive
     * expression using those operators, and an equivalent expression with
     * correct operator structure which is filled in after parsing.
     */
    case class Program(ops : Vector[(String, Int)], vars : Vector[VarDecl],
        expr : ExpR) extends TransformNode

    /**
     * A variable declaration.
     */
    case class VarDecl(name : String) extends TransformNode

    /**
     * Right recursive expression syntax class.
     */
    sealed abstract class ExpR extends TransformNode

    /**
     * Right recursive binary operator expression.
     */
    case class BinExpR(left : Exp, op : String, right : ExpR) extends ExpR

    /**
     * Primitive expression
     */
    case class Factor(exp : PrimExp) extends ExpR

    /**
     * Unrestricted expression syntax class.
     */
    sealed abstract class Exp extends TransformNode

    /**
     * Binary operator expression with arbitrary expression children.
     */
    case class BinExp(left : Exp, op : String, right : Exp) extends Exp

    /**
     * Primitive expression abstract class.
     */
    sealed abstract class PrimExp extends Exp

    /**
     * Integer literal expression.
     */
    case class Num(value : Int) extends PrimExp

    /**
     * Variable expression.
     */
    case class Var(name : String) extends PrimExp with EntityNode

}
