/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2013 Anthony M Sloane, Macquarie University.
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
package example.oneohonecompanies

object Precedence {

    import CompanyTree.{Company,Dept,Employee,CompanyTree,Salary}
    import Other.salary
    import org.kiama.attribution.Decorators.down
    import org.kiama.rewriting.Rewriter.everything

    /**
     * Return the salary of the boss of a particular part of a company,
     * or Float.MaxValue if there is no such boss.
     */
    val bosssalary : CompanyTree => Salary =
        down[CompanyTree,Salary] {
            case n if n.isRoot =>
                Float.MaxValue
            case Dept (_, m, _) =>
                m->salary
            case e : Employee =>
                e.parent[CompanyTree] match {
                    case p @ Dept (_, m, _) if m eq e =>
                        // Avoid comparing manager's salary with itself
                        p.parent[CompanyTree]->bosssalary
                    case p =>
                        p->bosssalary
                }
        }

    /**
     * Return true iff every employee has a salary no greater than
     * their boss.
     */
    def precedence (c : Company) : Boolean =
        everything (true) (_ && _) {
            case e : Employee => e->bosssalary >= e->salary
        } (c)

}
