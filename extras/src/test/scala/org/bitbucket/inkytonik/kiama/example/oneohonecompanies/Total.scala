/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oneohonecompanies

import org.bitbucket.inkytonik.kiama.attribution.Attribution

object Total extends Attribution {

    import CompanyTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.everything

    /**
     * Return the total salary cost of a company (by rewriting).
     */
    def total(c : Company) : Double =
        everything(0.0)(_ + _) { case s : Salary => s }(c)

}
