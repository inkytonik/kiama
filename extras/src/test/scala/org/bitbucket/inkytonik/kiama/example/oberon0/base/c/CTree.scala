/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package base.c

import org.bitbucket.inkytonik.kiama.output.PrettyExpression

/**
 * Root type of all C abstract syntax tree nodes.
 */
abstract class CNode

/**
 * C programs.
 */
case class CProgram(includes : Vector[CInclude], decls : Vector[CDeclaration]) extends CNode

/**
 * C include directive.
 */
case class CInclude(s : String) extends CNode

/**
 * Non-terminal type of C declarations.
 */
abstract class CDeclaration extends CNode

/**
 * C variable declarations.
 */
case class CVarDecl(ident : String, tipe : CType) extends CDeclaration

/**
 * C function declarations.
 */
case class CFunctionDecl(decl : CVarDecl, args : Vector[CDeclaration],
    body : CBlock) extends CDeclaration

/**
 * C blocks.
 */
case class CBlock(decls : Vector[CDeclaration], stmts : Vector[CStatement]) extends CStatement

/**
 * Non-terminal type for C types.
 */
abstract class CType extends CNode

/**
 * C integer type (int).
 */
case class CIntType() extends CType

/**
 * C string type (char *).
 */
case class CStrType() extends CType

/**
 * C array types.
 */
case class CArrayType(size : Int, elemtype : CType) extends CType

/**
 * Non-terminal type for C statements.
 */
abstract class CStatement extends CNode

/**
 * C empty statements.
 */
case class CEmptyStmt() extends CStatement

/**
 * C return statements.
 */
case class CReturn(e : CExpression) extends CStatement

/**
 * Non-terminal type for C expressions.
 */
abstract class CExpression extends CNode with PrettyExpression

/**
 * C integer expressions.
 */
case class CIntExp(v : Int) extends CExpression
