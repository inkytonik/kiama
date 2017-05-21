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

import CompanyTree.CompanyTree

class Precedence(tree : CompanyTree) {

    import CompanyTree.{Company, CompanyNode, Dept, Employee, Salary}
    import org.bitbucket.inkytonik.kiama.attribution.Decorators
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.everything

    val decorators = new Decorators(tree)
    import decorators.down

    val other = new Other(tree)
    import other.salary

    /**
     * Return the salary of the boss of a particular part of a company,
     * or Float.MaxValue if there is no such boss.
     */
    val bosssalary : CompanyNode => Salary =
        down[Salary](Float.MaxValue) {
            case Dept(_, m, _) =>
                salary(m)
            case tree.parent.pair(e : Employee, tree.parent.pair(Dept(_, m, _), p)) if m eq e =>
                // Avoid comparing manager's salary with itself
                bosssalary(p)
            case tree.parent.pair(_ : Employee, p) =>
                bosssalary(p)
        }

    /**
     * Return true iff every employee has a salary no greater than
     * their boss.
     */
    def precedence(c : Company) : Boolean =
        everything(true)(_ && _) {
            case e : Employee =>
                bosssalary(e) >= salary(e)
        }(c)

}
