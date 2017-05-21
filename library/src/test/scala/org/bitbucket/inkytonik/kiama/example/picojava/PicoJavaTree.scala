/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.bitbucket.inkytonik.kiama
package example.picojava

/**
 * PicoJava abstract syntax
 */
object PicoJavaTree {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    /**
     * Tree type for PicoJava programs.
     */
    type PicoJavaTree = Tree[PicoJavaNode, Program]

    // Created by parser
    sealed trait PicoJavaNode extends Product

    case class Program(Block : Block) extends PicoJavaNode

    case class Block(BlockStmts : Vector[BlockStmt]) extends PicoJavaNode
    sealed abstract class BlockStmt extends PicoJavaNode

    sealed abstract class Decl(val Name : String) extends BlockStmt
    sealed abstract class TypeDecl(Name : String) extends Decl(Name)
    case class ClassDecl(override val Name : String, Superclass : Option[IdnUse], Body : Block) extends TypeDecl(Name)
    case class VarDecl(Type : Access, override val Name : String) extends Decl(Name)

    sealed abstract class Stmt extends BlockStmt
    case class AssignStmt(Variable : Access, Value : Exp) extends Stmt
    case class WhileStmt(Condition : Exp, Body : Stmt) extends Stmt

    sealed abstract class Exp extends PicoJavaNode
    sealed abstract class Access extends Exp
    sealed abstract class IdnUse(val Name : String) extends Access

    case class Use(override val Name : String) extends IdnUse(Name)
    case class Dot(ObjectReference : Access, IdnUse : IdnUse) extends Access
    case class BooleanLiteral(Value : String) extends Exp

    // Created by NTA equations
    case class PrimitiveDecl(override val Name : String) extends TypeDecl(Name)
    case class UnknownDecl(override val Name : String) extends TypeDecl(Name)

}
