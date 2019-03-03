/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oneohonecompanies

import org.bitbucket.inkytonik.kiama.util.KiamaTests

class OneOhOneTests extends KiamaTests {

    import CompanyTree._
    import SampleCompany._
    import Total._
    import Cut.cut
    import Depth.depth

    {
        val empty = Company(Vector())

        val tree = new CompanyTree(empty)
        val othermod = new Other(tree)
        import othermod._
        val precedencemod = new Precedence(tree)
        import precedencemod._

        test("total of all salaries by rewriting - empty") {
            total(empty) shouldBe 0.0
        }

        test("total of all cut salaries by rewriting - empty") {
            total(cut(empty)) shouldBe 0.0
        }

        test("total of all salaries by attribution - empty") {
            salary(empty) shouldBe 0.0
        }

        test("total of all cut salaries by attribution - empty") {
            salary(cut(empty)) shouldBe 0.0
        }

        test("department depth - empty") {
            depth(empty) shouldBe 0
        }

        test("salaries ordered - empty") {
            precedence(empty) shouldBe true
        }
    }

    {
        val onlymanager = Company(Vector(Dept("D0", Employee("A", "Manager", 100), Vector())))

        val tree = new CompanyTree(onlymanager)
        val othermod = new Other(tree)
        import othermod._
        val precedencemod = new Precedence(tree)
        import precedencemod._

        test("total of all salaries by rewriting - onlymanager") {
            total(onlymanager) shouldBe 100.0
        }

        test("total of all cut salaries by rewriting - onlymanager") {
            total(cut(onlymanager)) shouldBe 50.0
        }

        test("total of all salaries by attribution - onlymanager") {
            salary(onlymanager) shouldBe 100.0
        }

        test("total of all cut salaries by attribution - onlymanager") {
            salary(cut(onlymanager)) shouldBe 50.0
        }

        test("department depth - onlymanager") {
            depth(onlymanager) shouldBe 1
        }

        test("salaries ordered - onlymanager") {
            precedence(onlymanager) shouldBe true
        }
    }

    {
        val tree = new CompanyTree(company)
        val othermod = new Other(tree)
        import othermod._
        val precedencemod = new Precedence(tree)
        import precedencemod._

        test("total of all salaries by rewriting - company") {
            total(company) shouldBe 399747.0
        }

        test("total of all cut salaries by rewriting - company") {
            total(cut(company)) shouldBe 199873.5
        }

        test("total of all salaries by attribution - company") {
            salary(company) shouldBe 399747.0
        }

        test("total of research salaries by attribution - company") {
            salary(research) shouldBe 137035.0
        }

        test("total of dev salaries by attribution - company") {
            salary(dev) shouldBe 262712.0
        }

        test("total of dev1 salaries by attribution - company") {
            salary(dev1) shouldBe 28145.0
        }

        test("total of dev11 salaries by attribution - company") {
            salary(dev11) shouldBe 4689.0
        }

        test("total of all cut salaries by attribution - company") {
            salary(cut(company)) shouldBe 199873.5
        }

        test("average salary for company") {
            averagesalary(company).toInt shouldBe 57106
        }

        test("average salary for research department") {
            averagesalary(research).toInt shouldBe 45678
        }

        test("average salary for dev department") {
            averagesalary(dev).toInt shouldBe 65678
        }

        test("average salary for dev1 department") {
            averagesalary(dev1).toInt shouldBe 9381
        }

        test("average salary for dev11 department") {
            averagesalary(dev11).toInt shouldBe 2344
        }

        test("craig is above average") {
            aboveaverage(craig) shouldBe true
        }

        test("klaus is above average") {
            aboveaverage(klaus) shouldBe true
        }

        test("joe is below average") {
            !aboveaverage(joe) shouldBe true
        }

        test("department depth - company") {
            depth(company) shouldBe 3
        }

        test("salaries ordered - company") {
            precedence(company) shouldBe true
        }

        test("company employee count is correct") {
            numemp(company) shouldBe 7
        }

        test("single employee count is correct") {
            numemp(craig) shouldBe 1
        }

        test("research department employee count is correct") {
            numemp(research) shouldBe 3
        }

        test("dev department employee count is correct") {
            numemp(dev) shouldBe 4
        }

        test("dev1 department employee count is correct") {
            numemp(dev1) shouldBe 3
        }

        test("dev11 department employee count is correct") {
            numemp(dev11) shouldBe 2
        }
    }

    {
        val d = Company(Vector(Dept("D1", Employee("An", "Emp", 100),
            Vector(PU(Employee("Another", "Emp", 500))))))

        val tree = new CompanyTree(d)
        val precedencemod = new Precedence(tree)
        import precedencemod.precedence

        test("salaries not ordered - employee") {
            precedence(d) shouldBe false
        }
    }

    {
        val d1 = Dept("D2", Employee("The", "Emp", 100),
            Vector(PU(Employee("That", "Emp", 50))))
        val d2 = Company(Vector(Dept("D3", Employee("TheOther", "Emp", 25),
            Vector(DU(d1)))))

        val tree = new CompanyTree(d2)
        val precedencemod = new Precedence(tree)
        import precedencemod.precedence

        test("salaries not ordered - manager") {
            precedence(d2) shouldBe false
        }
    }

}
