/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2015 Anthony M Sloane, Macquarie University.
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
    import scala.collection.immutable.Seq

    {
        val empty = Company (Nil)

        val tree = new CompanyTree (empty)
        val othermod = new Other (tree)
        import othermod._
        val precedencemod = new Precedence (tree)
        import precedencemod._

        test ("total of all salaries by rewriting - empty") {
            assertResult (0.0) (total (empty))
        }

        test ("total of all cut salaries by rewriting - empty") {
            assertResult (0.0) (total (cut (empty)))
        }

        test ("total of all salaries by attribution - empty") {
            assertResult (0.0) (salary (empty))
        }

        test ("total of all cut salaries by attribution - empty") {
            assertResult (0.0) (salary (cut (empty)))
        }

        test ("department depth - empty") {
            assertResult (0) (depth (empty))
        }

        test ("salaries ordered - empty") {
            assert (precedence (empty))
        }
    }

    {
        val onlymanager = Company (Seq (Dept ("D0", Employee ("A", "Manager", 100), Nil)))

        val tree = new CompanyTree (onlymanager)
        val othermod = new Other (tree)
        import othermod._
        val precedencemod = new Precedence (tree)
        import precedencemod._

        test ("total of all salaries by rewriting - onlymanager") {
            assertResult (100.0) (total (onlymanager))
        }

        test ("total of all cut salaries by rewriting - onlymanager") {
            assertResult (50.0) (total (cut (onlymanager)))
        }

        test ("total of all salaries by attribution - onlymanager") {
            assertResult (100.0) (salary (onlymanager))
        }

        test ("total of all cut salaries by attribution - onlymanager") {
            assertResult (50.0) (salary (cut (onlymanager)))
        }

        test ("department depth - onlymanager") {
            assertResult (1) (depth (onlymanager))
        }

        test ("salaries ordered - onlymanager") {
            assert (precedence (onlymanager))
        }
    }

    {
        val tree = new CompanyTree (company)
        val othermod = new Other (tree)
        import othermod._
        val precedencemod = new Precedence (tree)
        import precedencemod._

        test ("total of all salaries by rewriting - company") {
            assertResult (399747.0) (total (company))
        }

        test ("total of all cut salaries by rewriting - company") {
            assertResult (199873.5) (total (cut (company)))
        }

        test ("total of all salaries by attribution - company") {
            assertResult (399747.0) (salary (company))
        }

        test ("total of research salaries by attribution - company") {
            assertResult (137035.0) (salary (research))
        }

        test ("total of dev salaries by attribution - company") {
            assertResult (262712.0) (salary (dev))
        }

        test ("total of dev1 salaries by attribution - company") {
            assertResult (28145.0) (salary (dev1))
        }

        test ("total of dev11 salaries by attribution - company") {
            assertResult (4689.0) (salary (dev11))
        }

        test ("total of all cut salaries by attribution - company") {
            assertResult (199873.5) (salary (cut (company)))
        }

        test ("average salary for company") {
            assertResult (57106) (averagesalary (company).toInt)
        }

        test ("average salary for research department") {
            assertResult (45678) (averagesalary (research).toInt)
        }

        test ("average salary for dev department") {
            assertResult (65678) (averagesalary (dev).toInt)
        }

        test ("average salary for dev1 department") {
            assertResult (9381) (averagesalary (dev1).toInt)
        }

        test ("average salary for dev11 department") {
            assertResult (2344) (averagesalary (dev11).toInt)
        }

        test ("craig is above average") {
            assert (aboveaverage (craig))
        }

        test ("klaus is above average") {
            assert (aboveaverage (klaus))
        }

        test ("joe is below average") {
            assert (!aboveaverage (joe))
        }

        test ("department depth - company") {
            assertResult (3) (depth (company))
        }

        test ("salaries ordered - company") {
            assert (precedence (company))
        }

        test ("company employee count is correct") {
            assertResult (7) (numemp (company))
        }

        test ("single employee count is correct") {
            assertResult (1) (numemp (craig))
        }

        test ("research department employee count is correct") {
            assertResult (3) (numemp (research))
        }

        test ("dev department employee count is correct") {
            assertResult (4) (numemp (dev))
        }

        test ("dev1 department employee count is correct") {
            assertResult (3) (numemp (dev1))
        }

        test ("dev11 department employee count is correct") {
            assertResult (2) (numemp (dev11))
        }
    }

    {
        val d = Company (Seq (Dept ("D1", Employee ("An", "Emp", 100),
                                    Seq (PU (Employee ("Another", "Emp", 500))))))

        val tree = new CompanyTree (d)
        val precedencemod = new Precedence (tree)
        import precedencemod.precedence

        test ("salaries not ordered - employee") {
            assertResult (false) (precedence (d))
        }
    }

    {
        val d1 = Dept ("D2", Employee ("The", "Emp", 100),
                       Seq (PU (Employee ("That", "Emp", 50))))
        val d2 = Company (Seq (Dept ("D3", Employee ("TheOther", "Emp", 25),
                               Seq (DU (d1)))))

        val tree = new CompanyTree (d2)
        val precedencemod = new Precedence (tree)
        import precedencemod.precedence

        test ("salaries not ordered - manager") {
            assertResult (false) (precedence (d2))
        }
    }

}
