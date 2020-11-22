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
package example.oneohonecompanies

import CompanyTree.CompanyTree
import org.bitbucket.inkytonik.kiama.attribution.Attribution

class Other(tree : CompanyTree) extends Attribution {

    import CompanyTree._

    /**
     * Number of employees in a company unit.
     */
    lazy val numemp : CompanyNode => Int =
        attr {
            case Company(ds)     => (ds map numemp).sum
            case Dept(_, _, sus) => 1 + (sus map numemp).sum
            case DU(d)           => numemp(d)
            case _               => 1
        }

    /**
     * Total salary in a company unit.
     */
    lazy val salary : CompanyNode => Double =
        attr {
            case Company(ds)       => (ds map salary).sum
            case Dept(_, m, sus)   => m.s + (sus map salary).sum
            case PU(e)             => salary(e)
            case DU(d)             => salary(d)
            case Employee(_, _, s) => s
        }

    /**
     * Average salary for a department or company.
     */
    lazy val averagesalary : CompanyNode => Double =
        attr {
            case n @ (_ : Company | _ : Dept) =>
                salary(n) / numemp(n)
            case tree.parent(p) =>
                averagesalary(p)
            case n =>
                sys.error(s"averagesalary: unexpected CompanyNode $n")
        }

    lazy val aboveaverage : Employee => Boolean =
        attr {
            case e @ Employee(_, _, s) =>
                s > averagesalary(e)
        }

}
