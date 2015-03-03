/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.kiama
package example.picojava

/**
 * PicoJava abstract syntax
 */
object PicoJavaTree {

    import org.kiama.relation.Tree

    /**
     * Tree type for PicoJava programs.
     */
    type PicoJavaTree = Tree[PicoJavaNode,Program]

    // Created by parser
    sealed trait PicoJavaNode extends Product

    case class Program (Block : Block) extends PicoJavaNode

    case class Block (BlockStmts : List[BlockStmt]) extends PicoJavaNode
    sealed abstract class BlockStmt extends PicoJavaNode

    sealed abstract class Decl (val Name : String) extends BlockStmt
    sealed abstract class TypeDecl (Name : String) extends Decl (Name)
    case class ClassDecl (override val Name : String, Superclass : Option[IdnUse], Body : Block) extends TypeDecl (Name)
    case class VarDecl (Type : Access, override val Name : String) extends Decl (Name)

    sealed abstract class Stmt extends BlockStmt
    case class AssignStmt (Variable : Access, Value : Exp) extends Stmt
    case class WhileStmt (Condition : Exp, Body : Stmt) extends Stmt

    sealed abstract class Exp extends PicoJavaNode
    sealed abstract class Access extends Exp
    sealed abstract class IdnUse (val Name : String) extends Access

    case class Use (override val Name : String) extends IdnUse (Name)
    case class Dot (ObjectReference : Access, IdnUse : IdnUse) extends Access
    case class BooleanLiteral (Value : String) extends Exp

    // Created by NTA equations
    case class PrimitiveDecl (override val Name : String) extends TypeDecl (Name)
    case class UnknownDecl (override val Name : String) extends TypeDecl (Name)

}
