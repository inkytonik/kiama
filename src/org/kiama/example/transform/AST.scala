/*
 * AST for first-level of transformation compiler.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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
object AST {

    import org.kiama.attribution.Attributable

    /**
     * All AST nodes.
     */
    sealed abstract class ASTNode extends Attributable

    /**
     * Nodes that have entities associated with them.
     */
    trait EntityNode extends ASTNode

    /**
     * A program is a map from operator names to priorities, a right recursive
     * expression using those operators, and an equivalent expression with
     * correct operator structure which is filled in after parsing.
     */
    case class Program (ops : List[(String,Int)], vars : List[VarDecl],
                        expr : ExpR) extends ASTNode

    /**
     * A variable declaration.
     */
    case class VarDecl (name : String) extends ASTNode

    /**
     * Right recursive expression syntax class.
     */
    abstract class ExpR extends ASTNode

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
    abstract class Exp extends ASTNode

    /**
     * Binary operator expression with arbitrary expression children.
     */
    case class BinExp (left : Exp, op: String, right : Exp) extends Exp

    /**
     * Primitive expression abstract class.
     */
    abstract class PrimExp extends Exp

    /**
     * Integer literal expression.
     */
    case class Num (value : Int) extends PrimExp

    /**
     * Variable expression.
     */
    case class Var (name : String) extends PrimExp with EntityNode

}
