package org.kiama
package example.oberon0
package base.c

import org.kiama.output.PrettyExpression

/**
 * Root type of all C abstract syntax tree nodes.
 */
abstract class CASTNode

/**
 * C programs.
 */
case class CProgram (includes : List[CInclude], decls : List[CDeclaration]) extends CASTNode

/**
 * C include directive.
 */
case class CInclude (s : String) extends CASTNode

/**
 * Non-terminal type of C declarations.
 */
abstract class CDeclaration extends CASTNode

/**
 * C variable declarations.
 */
case class CVarDecl (ident : String, tipe : CType) extends CDeclaration

/**
 * C function declarations.
 */
case class CFunctionDecl (decl : CVarDecl, args : List[CDeclaration],
                          body : CBlock) extends CDeclaration

/**
 * C blocks.
 */
case class CBlock (decls : List[CDeclaration], stmts : List[CStatement]) extends CStatement

/**
 * Non-terminal type for C types.
 */
abstract class CType extends CASTNode

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
abstract class CStatement extends CASTNode

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
abstract class CExpression extends CASTNode with PrettyExpression

/**
 * C integer expressions.
 */
case class CIntExp (v : Int) extends CExpression
