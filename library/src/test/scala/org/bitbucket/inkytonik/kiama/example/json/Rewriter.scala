/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.json

object Rewriter {

    import JSONTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

    /**
     * Calculate total salary bill.
     */
    def total(c : JValue) : Double =
        everything(0.0)(_ + _) {
            case (JName("salary"), JNumber(d)) =>
                d
        }(c)

    /**
     * Cut all salaries by half.
     */
    def cut(c : JValue) : JValue =
        rewrite(everywhere(rule[(JName, JNumber)] {
            case (n @ JName("salary"), JNumber(d)) =>
                (n, JNumber(d / 2))
        }))(c)

}
