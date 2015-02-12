/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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
package example.repmin

import org.kiama.attribution.Attribution
import org.kiama.relation.Tree

/**
 * AST for Repmin examples.
 */
sealed abstract class RepminTree extends Product
case class Fork (left : RepminTree, right : RepminTree) extends RepminTree
case class Leaf (value : Int) extends RepminTree

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
            case Fork (l, r) => locmin (l).min (locmin (r))
            case Leaf (v)    => v
        }

    val repmin : RepminTree => RepminTree =
        attr {
            case Fork (l, r) => Fork (repmin (l), repmin (r))
            case t : Leaf    => Leaf (globmin (t))
        }

    val globmin : RepminTree => Int

}

/**
 * Classic repmin problem defined in an "attributes first" style.
 * repmin is a RepminTree with the same structure as its argument RepminTree
 * but with all of the leaves replaced by leaves containing the
 * minimum leaf value from the input Repmintree.
 */
class Repmin (tree : Tree[RepminTree,RepminTree]) extends RepminBase {

    val globmin : RepminTree => Int =
        attr {
            case tree.parent (p) =>
                globmin (p)
            case t =>
                locmin (t)
        }

}

/**
 * Repmin problem defined using decorators.
 */
class RepminDec (tree : Tree[RepminTree,RepminTree]) extends RepminBase {

    import org.kiama.attribution.Decorators

    val decorators = new Decorators (tree)

    val globmin : RepminTree => Int =
        decorators.atRoot (locmin)

}
