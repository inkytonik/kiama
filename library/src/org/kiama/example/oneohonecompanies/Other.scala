/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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

import CompanyTree.CompanyTree
import org.kiama.attribution.Attribution

class Other (tree : CompanyTree) extends Attribution {

    import CompanyTree._

    /**
     * Number of employees in a company unit.
     */
    lazy val numemp : CompanyNode => Int =
        attr {
            case Company (ds)     => (ds map numemp).sum
            case Dept (_, _, sus) => 1 + (sus map numemp).sum
            case DU (d)           => numemp (d)
            case _                => 1
        }

    /**
     * Total salary in a company unit.
     */
    lazy val salary : CompanyNode => Double =
        attr {
            case Company (ds)       => (ds map salary).sum
            case Dept (_, m, sus)   => m.s + (sus map salary).sum
            case PU (e)             => salary (e)
            case DU (d)             => salary (d)
            case Employee (_, _, s) => s
        }

    /**
     * Average salary for a department or company.
     */
    lazy val averagesalary : CompanyNode => Double =
        attr {
            case n @ (_ : Company | _ : Dept) =>
                salary (n) / numemp (n)
            case tree.parent (p) =>
                averagesalary (p)
        }

    lazy val aboveaverage : Employee => Boolean =
        attr {
            case e @ Employee (_, _, s) =>
                s > averagesalary (e)
        }

}
