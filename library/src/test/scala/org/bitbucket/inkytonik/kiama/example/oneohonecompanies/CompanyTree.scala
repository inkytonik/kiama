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

object CompanyTree {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    type CompanyTree = Tree[CompanyNode, Company]

    abstract class CompanyNode extends Product

    case class Company(depts : Vector[Dept]) extends CompanyNode
    case class Dept(n : Name, m : Manager, su : Vector[SubUnit]) extends CompanyNode

    type Manager = Employee
    case class Employee(n : Name, a : Address, s : Salary) extends CompanyNode

    abstract class SubUnit extends CompanyNode
    case class PU(e : Employee) extends SubUnit
    case class DU(d : Dept) extends SubUnit

    type Name = String
    type Address = String
    type Salary = Double

}
