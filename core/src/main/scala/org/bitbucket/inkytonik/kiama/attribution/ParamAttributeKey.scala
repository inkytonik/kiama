/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

/**
 * Support for parameterised attributes: argument, node pair comparison.
 */
class ParamAttributeKey(val arg : Any, val node : Any) {

    import org.bitbucket.inkytonik.kiama.util.Comparison.same

    override def equals(o : Any) : Boolean =
        o match {
            case o : ParamAttributeKey =>
                arg == o.arg && same(node, o.node)
            case _ =>
                false
        }

    override def hashCode : Int =
        System.identityHashCode(node) ^ arg.hashCode

}
