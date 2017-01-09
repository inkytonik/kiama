/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2017 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package relation

import scala.language.higherKinds

/**
 * A template trait for Relation-like types. `T` and `U` are the domain
 * and range types of the relation, respectively.
 */
trait RelationLike[T, U] {

    import org.bitbucket.inkytonik.kiama.util.Comparison.contains

    /**
     * The graph of this relation.
     */
    def graph : RelationGraph[T, U]

    /**
     * Apply this relation (same as `image`).
     */
    def apply(t : T) : Vector[U] =
        image(t)

    /**
     * Does the domain of this relation contain the value `t`?
     */
    def containsInDomain(t : T) : Boolean =
        graph.containsInDomain(t)

    /**
     * The domain of this relation.
     */
    lazy val domain : Vector[T] =
        graph.domain

    /**
     * The image of a value of the relation's domain is a set of the
     * values in the range that are related to that domain value.
     */
    def image(t : T) : Vector[U] =
        graph.image(t)

    /**
     * Invert this relation. In other words, if `(t,u)` is in the relation,
     * then `(u,t)` is in the inverted relation.
     */
    def inverse : RelationLike[U, T]

    /**
     * Is this relation empty (i.e., contains no pairs)?
     */
    lazy val isEmpty : Boolean =
        graph.size == 0

    /**
     * An auxiliary extractor for this relation that returns the matched
     * value `t` and its image as a sequence.
     */
    object pair {

        def unapplySeq(t : T) : Option[(T, Seq[U])] =
            Some((t, image(t)))

    }

    /**
     * The range of this relation.
     */
    lazy val range : Vector[U] =
        graph.range

    /**
     * A relation can be used as an extractor that matches the image of the
     * matched value `t`. E.g., the pattern `relation(a,b)` succeeds if and
     * only if the image of the matched value contains exactly two elements,
     * which are then bound to `a` and `b`, respectively. Normal sequence
     * matching works, such as `case relation(a, _*)` to match if there is
     * at least one element in the image and bind the first element to `a`.
     */
    def unapplySeq(t : T) : Option[Vector[U]] =
        Some(image(t))

}
