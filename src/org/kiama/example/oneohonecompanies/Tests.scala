/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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

import org.scalatest.FunSuite

class Tests extends FunSuite {
    
    import Company._
    import SampleCompany.company
    import Total.total
    import Cut.cut
    import Depth.depth
    import Precedence.precedence
    
    // Some pathological companies
    
    val empty = Company (Nil)
    val onlymanager = Company (List (Dept ("D0", Employee ("A", "Manager", 100), Nil)))
    
    test ("total of all salaries (company)") {
        expect (399747.0) (total (company))
    }
    
    test ("total of all salaries (empty)") {
        expect (0.0) (total (empty))
    }
    
    test ("total of all salaries (onlymanager)") {
        expect (100.0) (total (onlymanager))
    }
    
    test ("total of all cut salaries (company)") {
        expect (199873.5) (total (cut (company)))
    }
    
    test ("total of all cut salaries (empty)") {
        expect (0.0) (total (cut (empty)))
    }
    
    test ("total of all cut salaries (onlymanager)") {
        expect (50.0) (total (cut (onlymanager)))
    }
    
    test ("department depth (company)") {
        expect (3) (depth (company))
    }
    
    test ("department depth (empty)") {
        expect (0) (depth (empty))
    }
    
    test ("department depth (onlymanager)") {
        expect (1) (depth (onlymanager))
    }    
    
    test ("salaries ordered (company)") {
        expect (true) (precedence (company))
    }
    
    test ("salaries ordered (empty)") {
        expect (true) (precedence (empty))
    }
    
    test ("salaries ordered (onlymanager)") {
        expect (true) (precedence (onlymanager))
    }

    test ("salaries not ordered (employee)") {
        val d = Company (List (Dept ("D1", Employee ("An", "Emp", 100),
                               List (PU (Employee ("Another", "Emp", 500))))))
        expect (false) (precedence (d))
    }

    test ("salaries not ordered (manager)") {
        val d1 = Dept ("D2", Employee ("The", "Emp", 100),
                       List (PU (Employee ("That", "Emp", 50))))
        val d2 = Company (List (Dept ("D3", Employee ("TheOther", "Emp", 25), 
                               List (DU (d1)))))
        expect (false) (precedence (d2))
    }

}
