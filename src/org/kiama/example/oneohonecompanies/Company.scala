/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2012 Anthony M Sloane, Macquarie University.
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

object Company {
    
    import org.kiama.attribution.Attributable
    
    trait Node extends Attributable
    
    case class Company (depts : List[Dept]) extends Node
    case class Dept (n : Name, m : Manager, su : List[SubUnit]) extends Node

    type Manager = Employee
    case class Employee (n : Name, a : Address, s : Salary) extends Node

    abstract class SubUnit extends Node
    case class PU (e : Employee) extends SubUnit
    case class DU (d : Dept) extends SubUnit
    
    type Name = String
    type Address = String
    type Salary = Double

}
