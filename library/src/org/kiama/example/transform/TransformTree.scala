/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2015 Anthony M Sloane, Macquarie University.
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
package example.transform

/**
 * Abstract syntax.
 */
object TransformTree {

    import org.kiama.relation.Tree

    /**
     * Tree type for transform trees.
     */
    type TransformTree = Tree[TransformNode,Program]

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
    case class Program (ops : List[(String,Int)], vars : List[VarDecl],
                        expr : ExpR) extends TransformNode

    /**
     * A variable declaration.
     */
    case class VarDecl (name : String) extends TransformNode

    /**
     * Right recursive expression syntax class.
     */
    sealed abstract class ExpR extends TransformNode

    /**
     * Right recursive binary operator expression.
     */
    case class BinExpR (left : Exp, op: String, right : ExpR) extends ExpR

    /**
     * Primitive expression
     */
    case class Factor (exp : PrimExp) extends ExpR

    /**
     * Unrestricted expression syntax class.
     */
    sealed abstract class Exp extends TransformNode

    /**
     * Binary operator expression with arbitrary expression children.
     */
    case class BinExp (left : Exp, op: String, right : Exp) extends Exp

    /**
     * Primitive expression abstract class.
     */
   sealed abstract class PrimExp extends Exp

    /**
     * Integer literal expression.
     */
    case class Num (value : Int) extends PrimExp

    /**
     * Variable expression.
     */
    case class Var (name : String) extends PrimExp with EntityNode

}
