/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oneohonecompanies

object Depth {

    import CompanyTree.{Company, Dept}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.para

    /**
     * Return the nesting depth of departments.
     */
    def depth(c : Company) : Int =
        para[Int] {
            case (t, cs) =>
                (t match {
                    case d : Dept => 1
                    case _        => 0
                }) +
                    (cs :+ 0).max
        }(c)

}
