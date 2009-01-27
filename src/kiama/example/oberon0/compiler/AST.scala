/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
 *
 * Contributed by Ben Mockler.
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
 
package kiama.example.oberon0.compiler

/**
 * Abstract syntax for Oberon-0.
 */
object AST {
    
    abstract class Exp
    
    case class Ident (name: String) extends Exp
    case class Number (num: Int) extends Exp
    case class Not (e: Exp) extends Exp
    case class Pos (e: Exp) extends Exp
    case class Neg (e: Exp) extends Exp
    case class FieldDesig (left: Exp, id: Ident) extends Exp
    case class ArrayDesig (left: Exp, exp: Exp) extends Exp
    
    case class Mult (l: Exp, r: Exp) extends Exp
    case class Div (l: Exp, r: Exp) extends Exp
    case class Mod (l: Exp, r: Exp) extends Exp
    case class And (l: Exp, r: Exp) extends Exp
    case class Plus (l: Exp, r: Exp) extends Exp
    case class Minus (l: Exp, r: Exp) extends Exp
    case class Or (l: Exp, r: Exp) extends Exp
    case class Equal (l: Exp, r: Exp) extends Exp
    case class NotEqual (l: Exp, r: Exp) extends Exp
    case class LessThan (l: Exp, r: Exp) extends Exp
    case class LessThanOrEqual (l: Exp, r: Exp) extends Exp
    case class GreaterThan (l: Exp, r: Exp) extends Exp
    case class GreaterThanOrEqual (l: Exp, r: Exp) extends Exp
    
    abstract class Statement
    
    case class Assignment (desig: Exp, exp: Exp) extends Statement
    case class ProcedureCall (desig: Exp, aps: List[Exp]) extends Statement
    case class IfStatement (condexp: Exp, thenstmts: List[Statement], elsestmts: List[Statement]) extends Statement
    case class WhileStatement (condexp: Exp, bodystmts: List[Statement]) extends Statement
    
    abstract class Declaration (id: Ident)
    
    case class ConstDecl (id: Ident, constval: Exp) extends Declaration (id)
    case class VarDecl (id: Ident, tp: Type) extends Declaration (id)
    case class RefVarDecl (id: Ident, tp: Type) extends Declaration (id)
    case class TypeDecl (id: Ident, tp: Type) extends Declaration (id)
    case class FieldDecl (id: Ident, tp: Type) extends Declaration (id)
    case class ModuleDecl (id: Ident, decls: List[Declaration], stmts: List[Statement], id2: Ident) extends Declaration (id)
    case class ProcDecl (id: Ident, fps: List[Declaration], decls: List[Declaration], stmts: List[Statement], id2: Ident) extends Declaration (id)
    
    abstract class Type
    
    case class NamedType (id: Ident) extends Type
    case class ArrayType (size: Exp, tp: Type) extends Type
    case class RecordType (fldlst: List[FieldDecl]) extends Type
    
}
