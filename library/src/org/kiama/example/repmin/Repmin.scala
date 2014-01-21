/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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

import org.kiama.util.TreeNode
import org.kiama.attribution.Attribution._

/**
 * AST for Repmin examples.
 */
sealed abstract class RepminTree extends TreeNode
case class Fork (left : RepminTree, right : RepminTree) extends RepminTree
case class Leaf (value : Int) extends RepminTree

/**
 * Repmin implementations must provide a repmin attribute.
 */
trait RepminImpl {
    val repmin : RepminTree => RepminTree
}

/**
 * Common base for Repmin implementations.
 */
trait RepminBase extends RepminImpl {

    val locmin : RepminTree => Int =
        attr {
            case Fork (l, r) => (l->locmin) min (r->locmin)
            case Leaf (v)    => v
        }

    val repmin : RepminTree => RepminTree =
        attr {
            case Fork (l, r) => Fork (l->repmin, r->repmin)
            case t : Leaf    => Leaf (t->globmin)
        }

    val globmin : RepminTree => Int

}

/**
 * Classic repmin problem defined in an "attributes first" style.
 * repmin is a Repmintree with the same structure as its argument Repmintree
 * but with all of the leaves replaced by leaves containing the
 * minimum leaf value from the input Repmintree.
 */
trait Repmin extends RepminBase {

    val globmin : RepminTree => Int =
        attr {
            case t if t.isRoot => t->locmin
            case t             => t.parent[RepminTree]->globmin
        }

}

/**
 * Repmin problem defined using decorators.
 */
trait RepminDec extends RepminBase {

    import org.kiama.attribution.Decorators._

    val globmin : RepminTree => Int =
        downErr[RepminTree,Int] {
            case t if t.isRoot => t->locmin
        }

}

