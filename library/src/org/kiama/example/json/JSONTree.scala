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
package example.json

/**
 * Module containing structures for representing JSON programs.
 */
object JSONTree {

    import org.kiama.util.Tree
    import scala.collection.immutable.Seq

    /**
     * Interface for all JSON tree nodes.
     */
    sealed abstract class JValue extends Tree

    /**
     * A JSON object.
     */
    case class JObject (fields : Seq[(JName,JValue)]) extends JValue

    /**
     * The name of a JSON field.
     */
    case class JName (s : String) extends Tree

    /**
     * A JSON array.
     */
    case class JArray (values : Seq[JValue]) extends JValue

    /**
     * A JSON string value.
     */
    case class JString (value : String) extends JValue

    /**
     * A JSON number value.
     */
    case class JNumber (value : Double) extends JValue

    /**
     * A JSON true value.
     */
    case class JTrue () extends JValue

    /**
     * A JSON false value.
     */
    case class JFalse () extends JValue

    /**
     * A JSON null value.
     */
    case class JNull () extends JValue

}
