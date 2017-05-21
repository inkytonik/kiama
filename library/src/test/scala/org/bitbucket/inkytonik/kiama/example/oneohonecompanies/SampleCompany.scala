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

object SampleCompany {

    import CompanyTree._

    val craig = Employee("Craig", "Redmond", 123456)
    val erik = Employee("Erik", "Utrecht", 12345)
    val ralf = Employee("Ralf", "Koblenz", 1234)
    val research = Dept("Research", craig, Vector(PU(erik), PU(ralf)))

    val klaus = Employee("Klaus", "Boston", 23456)
    val ray = Employee("Ray", "Redmond", 234567)
    val karl = Employee("Karl", "Riga", 2345)
    val joe = Employee("Joe", "Wifi City", 2344)
    val dev11 = Dept("Dev1.1", karl, Vector(PU(joe)))
    val dev1 = Dept("Dev1", klaus, Vector(DU(dev11)))
    val dev = Dept("Development", ray, Vector(DU(dev1)))

    val company = Company(Vector(research, dev))

}
