/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package example.oberon0
package base.c

import org.kiama.output.PrettyExpression
import scala.collection.immutable.Seq

/**
 * Root type of all C abstract syntax tree nodes.
 */
abstract class CTree

/**
 * C programs.
 */
case class CProgram (includes : Seq[CInclude], decls : Seq[CDeclaration]) extends CTree

/**
 * C include directive.
 */
case class CInclude (s : String) extends CTree

/**
 * Non-terminal type of C declarations.
 */
abstract class CDeclaration extends CTree

/**
 * C variable declarations.
 */
case class CVarDecl (ident : String, tipe : CType) extends CDeclaration

/**
 * C function declarations.
 */
case class CFunctionDecl (decl : CVarDecl, args : Seq[CDeclaration],
                          body : CBlock) extends CDeclaration

/**
 * C blocks.
 */
case class CBlock (decls : Seq[CDeclaration], stmts : Seq[CStatement]) extends CStatement

/**
 * Non-terminal type for C types.
 */
abstract class CType extends CTree

/**
 * C integer type (int).
 */
case class CIntType () extends CType

/**
 * C string type (char *).
 */
case class CStrType () extends CType

/**
 * C array types.
 */
case class CArrayType (size : Int, elemtype : CType) extends CType

/**
 * Non-terminal type for C statements.
 */
abstract class CStatement extends CTree

/**
 * C empty statements.
 */
case class CEmptyStmt () extends CStatement

/**
 * C return statements.
 */
case class CReturn (e : CExpression) extends CStatement

/**
 * Non-terminal type for C expressions.
 */
abstract class CExpression extends CTree with PrettyExpression

/**
 * C integer expressions.
 */
case class CIntExp (v : Int) extends CExpression
