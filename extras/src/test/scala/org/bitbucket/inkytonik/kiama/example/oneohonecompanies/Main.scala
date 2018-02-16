/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oneohonecompanies

object Main {

    import org.bitbucket.inkytonik.kiama.util.OutputEmitter
    import CompanyTree.CompanyTree
    import SampleCompany.company

    def main(args : Array[String]) {
        val output = new OutputEmitter
        val tree = new CompanyTree(company)
        output.emitln(Total.total(company))
        output.emitln(Total.total(Cut.cut(company)))
        output.emitln(Depth.depth(company))
        output.emitln(new Precedence(tree).precedence(company))
    }

}
