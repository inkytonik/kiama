/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 * 
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava

/**
 * PicoJava abstract syntax
 */
object AbstractSyntax {

    // Created by parser
    case class Program (Block : Block)  // FIXME: /PredefinedType:TypeDecl* /

    case class Block (BlockStmts : Seq[BlockStmt])
    abstract class BlockStmt

    abstract class Decl (Name : String) extends BlockStmt
    abstract class TypeDecl (Name : String) extends Decl (Name)
    case class ClassDecl (Name : String, Superclass : Option[IdUse], Body : Block) extends TypeDecl (Name)
    case class VarDecl (Name : String, Type : Access) extends Decl (Name)

    abstract class Stmt extends BlockStmt
    case class AssignStmt (Variable : Access, Value : Exp) extends Stmt
    case class WhileStmt (Condition : Exp, Body : Stmt) extends Stmt

    abstract class Exp
    abstract class Access extends Exp
    abstract class IdUse (Name : String) extends Access
    
    case class Use (Name : String) extends IdUse (Name)
    case class Dot (ObjectReference : Access, IdUse : IdUse) extends Access
    case class BooleanLiteral (Value : String) extends Exp

    // Created by NTA equations
    case class PrimitiveDecl (Name : String) extends TypeDecl (Name)
    case class UnknownDecl (Name : String) extends TypeDecl (Name)

    // Created by Rewrites
    case class TypeUse (Name : String) extends IdUse (Name)
    case class VariableUse (Name : String) extends IdUse (Name)
  
}
