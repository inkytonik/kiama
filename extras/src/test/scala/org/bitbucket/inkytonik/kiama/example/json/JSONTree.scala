/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.json

/**
 * Module containing structures for representing JSON programs.
 */
object JSONTree {

    /**
     * Base type for all JSON tree nodes.
     */
    sealed abstract class JSONNode

    /**
     * Interface for all JSON tree nodes.
     */
    sealed abstract class JValue extends JSONNode

    /**
     * A JSON object.
     */
    case class JObject(fields : Vector[(JName, JValue)]) extends JValue

    /**
     * The name of a JSON field.
     */
    case class JName(s : String) extends JSONNode

    /**
     * A JSON array.
     */
    case class JArray(values : Vector[JValue]) extends JValue

    /**
     * A JSON string value.
     */
    case class JString(value : String) extends JValue

    /**
     * A JSON number value.
     */
    case class JNumber(value : Double) extends JValue

    /**
     * A JSON true value.
     */
    case class JTrue() extends JValue

    /**
     * A JSON false value.
     */
    case class JFalse() extends JValue

    /**
     * A JSON null value.
     */
    case class JNull() extends JValue

}
