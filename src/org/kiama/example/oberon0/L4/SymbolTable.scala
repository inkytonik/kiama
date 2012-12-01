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
package L4

trait SymbolTable extends L3.SymbolTable {

    /**
     * An array type with the given size and element type.  Setting size
     * to zero and elemtype to the unknown type means an arbitrary array
     * type.
     */
    case class ArrayType (size : Int, elemtype : Type) extends Type {
        override def toString : String = "ARRAY " + size + " OF " + elemtype
    }

    /**
     * A record with the given fields.
     */
    case class RecordType (fields : List[Field]) extends Type {
        override def toString : String = "RECORD " + fields.mkString ("; ") + " END"
    }

    /**
     * A record field.
     */
    case class Field (ident : String, tipe : Type) extends Entity {
        override def toString : String = ident + " : " + tipe
    }

    /**
     * Return true if the entity is erroneous or is an array.
     */
    def isArray (e : Entity) : Boolean =
        isError (e) || e.isInstanceOf[ArrayType]

    /**
     * Return true if the entity is erroneous or is not an array.
     */
    def isNotArray (e : Entity) : Boolean =
        isError (e) || !e.isInstanceOf[ArrayType]

    /**
     * Return true if the entity is erroneous or is a record.
     */
    def isRecord (e : Entity) : Boolean =
        isError (e) || e.isInstanceOf[RecordType]

    /**
     * Return true if the entity is erroneous or is not a record.
     */
    def isNotRecord (e : Entity) : Boolean =
        isError (e) || !e.isInstanceOf[RecordType]

    /**
     * Is a given type a record type containing a field called f?
     */
    def hasField (t : Type, f : String) : Boolean =
        t match {
            case RecordType (fs) => fs.map(_.ident) contains f
            case _               => false
        }

}
