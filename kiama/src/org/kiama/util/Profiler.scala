/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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
package util

/**
 * Kiama-specific additions to `dsprofile` profilers.
 */
trait Profiler extends org.bitbucket.inkytonik.dsprofile.Profiler {

    import org.bitbucket.inkytonik.dsprofile.Events.{Dimension, Value}
    import org.kiama.attribution.Attribute
    import org.kiama.rewriting.Strategy

    /**
     * Support Kiama-specific profilnig dimensions.
     */
    override def dimValue (record : Record, dim : Dimension) : Value =
        dim match {

            /**
             * The `name` dimension is the string that identifies either the
             * strategy or attribute that is being profiled. They are checked
             * in that order.
             */
            case "name" =>
                checkFor (record, dim, "StratEval", "strategy") {
                    case s : Strategy =>
                        s.name
                    case _ =>
                        checkFor (record, dim, "AttrEval", "attribute") {
                            case a : Attribute[_,_] =>
                                a.name
                            case _ =>
                                "unknown name"
                        }
                }

            case _ =>
                super.dimValue (record, dim)

        }

}
