/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L2

trait NameAnalyser extends L0.NameAnalyser {

    import base.source.Expression
    import source.{ForStatement, MinMaxCond, ValCond}

    override def rootconstexpDef : Expression => Boolean =
        {
            case e1 @ tree.parent(ForStatement(_, _, _, Some(e2), _)) if e1 == e2 =>
                true
            case tree.parent(_ : ValCond | _ : MinMaxCond) =>
                true
            case e =>
                super.rootconstexpDef(e)
        }

}
