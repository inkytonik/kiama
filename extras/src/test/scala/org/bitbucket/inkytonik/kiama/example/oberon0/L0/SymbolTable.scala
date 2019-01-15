/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L0

trait SymbolTable extends base.SymbolTable {

    import base.Oberon0Entity
    import base.source.ModuleDecl
    import source.{ConstDecl, TypeDecl, TypeDef}

    /**
     * A user-defined module represented by a module declaration.
     */
    case class Module(ident : String, decl : ModuleDecl) extends Oberon0Entity with Named

    /**
     * A user-defined constant entity represented by a constant declaration.
     */
    case class Constant(ident : String, decl : ConstDecl) extends Oberon0Entity with Named

    /**
     * A variable entity including a reference to its types' definition.
     */
    case class Variable(ident : String, tipe : TypeDef) extends Oberon0Entity with Named

    /**
     * An entity representing by a user-provided type declaration.
     */
    abstract class Type extends Oberon0Entity

    /**
     * A user-defined type.
     */
    case class UserType(ident : String, tipe : TypeDecl) extends Type with Named

    /**
     * A built-in type with an implicit definition that the compiler must have
     * special knowledge about. This mechanism is necessary since the built-in
     * types cannot be defined using source concepts.
     */
    case class BuiltinType(ident : String) extends Type {
        override def toString = ident
    }

    /**
     * A type that is unknown, eg because the typed thing is erroneously
     * defined.
     */
    lazy val unknownType = BuiltinType("unknown")

    /**
     * Built-in integer type.
     */
    lazy val integerType = BuiltinType("INTEGER")

    /**
     * Built-in Boolean type.
     */
    lazy val booleanType = BuiltinType("BOOLEAN")

    /**
     * A built-in value of some type that is represented by a particular integer
     * value. The type does not have to be INTEGER. In other words, this value
     * cannot be defined using a constant declaration so the compiler has to
     * have special knowledge of them.
     */
    case class IntegerValue(ident : String, tipe : Type, value : Int) extends Oberon0Entity

    /**
     * Built-in true constant.
     */
    lazy val trueConstant = IntegerValue("TRUE", booleanType, 1)

    /**
     * Built-in false constant.
     */
    lazy val falseConstant = IntegerValue("FALSE", booleanType, 0)

    /**
     * The default environment.
     */
    def defenv : Environment =
        rootenv(defenvPairs : _*)

    def defenvPairs : List[(String, Oberon0Entity)] =
        List(
            "INTEGER" -> integerType,
            "BOOLEAN" -> booleanType,
            "TRUE" -> trueConstant,
            "FALSE" -> falseConstant
        )

    /**
     * Return true if the entity is a builtin, false otherwise.
     */
    def isBuiltin(e : Oberon0Entity) : Boolean =
        e.isInstanceOf[BuiltinType] || e.isInstanceOf[IntegerValue]

    /**
     * Return true if the entity is an error, false otherwise.
     */
    def isError(e : Oberon0Entity) : Boolean =
        e.isInstanceOf[MultipleEntity] || e.isInstanceOf[UnknownEntity]

    /**
     * Return true if the entity is erroneous or is a module.
     */
    def isModule(e : Oberon0Entity) : Boolean =
        isError(e) || e.isInstanceOf[Module]

    /**
     * Return true if the entity is erroneous or is a constant.
     */
    def isConstant(e : Oberon0Entity) : Boolean =
        isError(e) || e.isInstanceOf[Constant] || e.isInstanceOf[IntegerValue]

    /**
     * Return true if the entity is erroneous or is a type.
     */
    def isType(e : Oberon0Entity) : Boolean =
        isError(e) || e.isInstanceOf[Type]

    /**
     * Return true if the entity is erroneous or is a variable.
     */
    def isVariable(e : Oberon0Entity) : Boolean =
        isError(e) || e.isInstanceOf[Variable]

    /**
     * Return true if the given type is integer or an unknown type.
     */
    def isInteger(e : Type) : Boolean =
        (e == integerType) || (e == unknownType)

    /**
     * Return true if the given type is Boolean or an unknown type.
     */
    def isBoolean(e : Type) : Boolean =
        (e == booleanType) || (e == unknownType)

}
