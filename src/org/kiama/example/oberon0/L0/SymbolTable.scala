/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package L0

trait SymbolTable extends base.SymbolTable {

    import base.source.ModuleDecl
    import scala.collection.immutable.Stack
    import source.{ConstDecl, TypeDecl, TypeDef}

    /**
     * A user-defined module represented by a module declaration.
     */
    case class Module (ident : String, decl : ModuleDecl) extends NamedEntity

    /**
     * A user-defined constant entity represented by a constant declaration.
     */
    case class Constant (ident : String, decl : ConstDecl) extends NamedEntity

    /**
     * A variable entity including a reference to its types' definition.
     */
    case class Variable (ident : String, tipe : TypeDef) extends NamedEntity

    /**
     * An entity representing by a user-provided type declaration.
     */
    abstract class Type extends Entity

    /**
     * A user-defined type.
     */
    case class UserType (ident : String, tipe : TypeDecl) extends Type with Named

    /**
     * Marker trait for all built-in entities.
     */
    trait Builtin extends Named {
        this : Entity =>
    }

    /**
     * A built-in type with an implicit definition that the compiler must have
     * special knowledge about. This mechanism is necessary since the built-in
     * types cannot be defined using source concepts.
     */
    case class BuiltinType (ident : String) extends Type with Named with Builtin

    /**
     * A type that is unknown, eg because the typed thing is erroneously
     * defined.
     */
    lazy val unknownType = BuiltinType ("unknown")

    /**
     * Built-in integer type.
     */
    lazy val integerType = BuiltinType ("INTEGER")

    /**
     * Built-in Boolean type.
     */
    lazy val booleanType = BuiltinType ("BOOLEAN")

    /**
     * A built-in value of some type that is represented by a particular integer
     * value. The type does not have to be INTEGER. In other words, this value
     * cannot be defined using a constant declaration so the compiler has to
     * have special knowledge of them.
     */
    case class IntegerValue (ident : String, tipe : Type, value : Int) extends NamedEntity with Builtin

    /**
     * Built-in true constant.
     */
    lazy val trueConstant = IntegerValue ("TRUE", booleanType, 1)

    /**
     * Built-in false constant.
     */
    lazy val falseConstant = IntegerValue ("FALSE", booleanType, 0)

    /**
     * The default environment.
     */
    def defenv : Environment =
        rootenv (defenvPairs : _*)

    def defenvPairs : List[(String,Entity)] =
        List (
            "INTEGER" -> integerType,
            "BOOLEAN" -> booleanType,
            "TRUE" -> trueConstant,
            "FALSE" -> falseConstant
        )

    /**
     * Return true if the entity is an error, false otherwise.
     */
    def isError (e : Entity) : Boolean =
        e.isInstanceOf[ErrorEntity]

    /**
     * Return true if the entity is erroneous or is a module.
     */
    def isModule (e : Entity) : Boolean =
        isError (e) || e.isInstanceOf[Module]

    /**
     * Return true if the entity is erroneous or is a constant.
     */
    def isConstant (e : Entity) : Boolean =
        isError (e) || e.isInstanceOf[Constant] || e.isInstanceOf[IntegerValue]

    /**
     * Return true if the entity is erroneous or is a type.
     */
    def isType (e : Entity) : Boolean =
        isError (e) || e.isInstanceOf[Type]

    /**
     * Return true if the entity is erroneous or is a variable.
     */
    def isVariable (e : Entity) : Boolean =
        isError (e) || e.isInstanceOf[Variable]

    /**
     * Return true if the given type is integer or an unknown type.
     */
    def isInteger (e : Type) : Boolean =
        (e == integerType) || (e == unknownType)

    /**
     * Return true if the given type is Boolean or an unknown type.
     */
    def isBoolean (e : Type) : Boolean =
        (e == booleanType) || (e == unknownType)

}
