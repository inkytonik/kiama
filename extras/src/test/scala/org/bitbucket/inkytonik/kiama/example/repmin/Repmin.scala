/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.repmin

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree

/**
 * AST for Repmin examples.
 */
sealed abstract class RepminTree extends Product
case class Fork(left : RepminTree, right : RepminTree) extends RepminTree
case class Leaf(value : Int) extends RepminTree

/**
 * Repmin implementations must provide a repmin attribute.
 */
trait RepminImpl extends Attribution {
    val repmin : RepminTree => RepminTree
}

/**
 * Common base for Repmin implementations.
 */
trait RepminBase extends RepminImpl {

    val locmin : RepminTree => Int =
        attr {
            case Fork(l, r) => locmin(l).min(locmin(r))
            case Leaf(v)    => v
        }

    val repmin : RepminTree => RepminTree =
        attr {
            case Fork(l, r) => Fork(repmin(l), repmin(r))
            case t : Leaf   => Leaf(globmin(t))
        }

    val globmin : RepminTree => Int

}

/**
 * Classic repmin problem defined in an "attributes first" style.
 * repmin is a RepminTree with the same structure as its argument RepminTree
 * but with all of the leaves replaced by leaves containing the
 * minimum leaf value from the input Repmintree.
 */
class Repmin(tree : Tree[RepminTree, RepminTree]) extends RepminBase {

    val globmin : RepminTree => Int =
        attr {
            case tree.parent(p) =>
                globmin(p)
            case t =>
                locmin(t)
        }

}

/**
 * Repmin problem defined using decorators.
 */
class RepminDec(tree : Tree[RepminTree, RepminTree]) extends RepminBase {

    import org.bitbucket.inkytonik.kiama.attribution.Decorators

    val decorators = new Decorators(tree)

    val globmin : RepminTree => Int =
        decorators.atRoot(locmin)

}
