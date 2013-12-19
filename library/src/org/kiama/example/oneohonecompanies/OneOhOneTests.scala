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

import org.kiama.util.Tests

class OneOhOneTests extends Tests {

    import CompanyTree._
    import SampleCompany._
    import Total._
    import Cut.cut
    import Depth.depth
    import Precedence.precedence
    import Other._
    import org.kiama.attribution.Attribution.initTree
    import scala.collection.immutable.Seq

    // Some pathological companies

    val empty = Company (Nil)
    val onlymanager = Company (Seq (Dept ("D0", Employee ("A", "Manager", 100), Nil)))

    test ("total of all salaries by rewriting - company") {
        assertResult (399747.0) (total (company))
    }

    test ("total of all salaries by rewriting - empty") {
        assertResult (0.0) (total (empty))
    }

    test ("total of all salaries by rewriting - onlymanager") {
        assertResult (100.0) (total (onlymanager))
    }

    test ("total of all cut salaries by rewriting - company") {
        assertResult (199873.5) (total (cut (company)))
    }

    test ("total of all cut salaries by rewriting - empty") {
        assertResult (0.0) (total (cut (empty)))
    }

    test ("total of all cut salaries by rewriting - onlymanager") {
        assertResult (50.0) (total (cut (onlymanager)))
    }

    test ("total of all salaries by attribution - company") {
        assertResult (399747.0) (company->salary)
    }

    test ("total of research salaries by attribution - company") {
        assertResult (137035.0) (research->salary)
    }

    test ("total of dev salaries by attribution - company") {
        assertResult (262712.0) (dev->salary)
    }

    test ("total of dev1 salaries by attribution - company") {
        assertResult (28145.0) (dev1->salary)
    }

    test ("total of dev11 salaries by attribution - company") {
        assertResult (4689.0) (dev11->salary)
    }

    test ("total of all salaries by attribution - empty") {
        assertResult (0.0) (empty->salary)
    }

    test ("total of all salaries by attribution - onlymanager") {
        assertResult (100.0) (onlymanager->salary)
    }

    test ("total of all cut salaries by attribution - company") {
        assertResult (199873.5) ((cut (company)->salary))
    }

    test ("total of all cut salaries by attribution - empty") {
        assertResult (0.0) ((cut (empty)->salary))
    }

    test ("total of all cut salaries by attribution - onlymanager") {
        assertResult (50.0) ((cut (onlymanager)->salary))
    }

    test ("average salary for company") {
        assertResult (57106) ((company->averagesalary).toInt)
    }

    test ("average salary for research department") {
        assertResult (45678) ((research->averagesalary).toInt)
    }

    test ("average salary for dev department") {
        assertResult (65678) ((dev->averagesalary).toInt)
    }

    test ("average salary for dev1 department") {
        assertResult (9381) ((dev1->averagesalary).toInt)
    }

    test ("average salary for dev11 department") {
        assertResult (2344) ((dev11->averagesalary).toInt)
    }

    test ("craig is above average") {
        assertResult (true) (craig->aboveaverage)
    }

    test ("klaus is above average") {
        assertResult (true) (klaus->aboveaverage)
    }

    test ("joe is below average") {
        assertResult (false) (joe->aboveaverage)
    }

    test ("department depth - company") {
        assertResult (3) (depth (company))
    }

    test ("department depth - empty") {
        assertResult (0) (depth (empty))
    }

    test ("department depth - onlymanager") {
        assertResult (1) (depth (onlymanager))
    }

    test ("salaries ordered - company") {
        assertResult (true) (precedence (company))
    }

    test ("salaries ordered - empty") {
        assertResult (true) (precedence (empty))
    }

    test ("salaries ordered - onlymanager") {
        assertResult (true) (precedence (onlymanager))
    }

    test ("salaries not ordered - employee") {
        val d = Company (Seq (Dept ("D1", Employee ("An", "Emp", 100),
                               Seq (PU (Employee ("Another", "Emp", 500))))))
        initTree (d)
        assertResult (false) (precedence (d))
    }

    test ("salaries not ordered - manager") {
        val d1 = Dept ("D2", Employee ("The", "Emp", 100),
                       Seq (PU (Employee ("That", "Emp", 50))))
        val d2 = Company (Seq (Dept ("D3", Employee ("TheOther", "Emp", 25),
                               Seq (DU (d1)))))
        initTree (d2)
        assertResult (false) (precedence (d2))
    }

    test ("company employee count is correct") {
        assertResult (7) (company->numemp)
    }

    test ("single employee count is correct") {
        assertResult (1) (craig->numemp)
    }

    test ("research department employee count is correct") {
        assertResult (3) (research->numemp)
    }

    test ("dev department employee count is correct") {
        assertResult (4) (dev->numemp)
    }

    test ("dev1 department employee count is correct") {
        assertResult (3) (dev1->numemp)
    }

    test ("dev11 department employee count is correct") {
        assertResult (2) (dev11->numemp)
    }

}
