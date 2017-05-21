/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oneohonecompanies

object Cut {

    import CompanyTree.{Company, Salary}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywhere, rewrite, rule}

    /**
     * Reduce all salaries by half.
     */
    def cut(c : Company) : Company =
        rewrite(everywhere(rule[Salary] { case s => s / 2 }))(c)

}
