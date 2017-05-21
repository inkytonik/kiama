/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package relation

import org.bitbucket.inkytonik.kiama.util.Memoiser
import org.bitbucket.inkytonik.kiama.util.Memoiser.makeIdMemoiser

/**
 * A binary relation between values of type `T` and values of type `U`.
 * Constructed from memoised caches that map `T` values to their image
 * and vice versa.
 */
class Relation[T, U](
        val graph : Memoiser[T, Vector[U]] = makeIdMemoiser[T, Vector[U]](),
        val inverseGraph : Memoiser[U, Vector[T]] = makeIdMemoiser[U, Vector[T]]()
) {

    rel =>

    import org.bitbucket.inkytonik.kiama.relation.Relation.emptyImage

    /**
     * An accessor for the graph field for those rare cases where a
     * client needs to get to it.
     */
    def getGraph =
        graph

    /**
     * The image of a value of the relation's domain is a set of the
     * values in the range that are related to that domain value.
     */
    def apply(t : T) : Vector[U] =
        graph.getOrDefault(t, emptyImage)

    /**
     * Does the domain of this relation contain the value `t`?
     */
    def containsInDomain(t : T) : Boolean =
        graph.hasBeenComputedAt(t)

    /**
     * The domain of this relation.
     */
    def domain : Vector[T] =
        graph.keys

    /**
     * Return a relation that is the inverse of this one. In other
     * words, if `(t,u)` is in the relation, then `(u,t)` is in the
     * inverted relation.
     */
    lazy val inverse : Relation[U, T] =
        new Relation(inverseGraph, graph) {
            override lazy val inverse : Relation[T, U] =
                rel
        }

    /**
     * Is this relation empty (i.e., contains no pairs)?
     */
    def isEmpty : Boolean =
        graph.size() == 0

    /**
     * An auxiliary extractor for this relation that returns the matched
     * value `t` and its image as a sequence.
     */
    object pair {

        def unapplySeq(t : T) : Option[(T, Seq[U])] =
            Some((t, apply(t)))

    }

    /**
     * Add the pair `(t,u)`` to the relation and the pair `(u,t)` to
     * its inverse.
     */
    def put(t : T, u : U) {
        graph.updateAt(t, _ :+ u, Vector(u))
        inverseGraph.updateAt(u, _ :+ t, Vector(t))
    }

    /**
     * The range of this relation.
     */
    def range : Vector[U] =
        inverseGraph.keys

    /**
     * The size of this relation.
     */
    def size : Long =
        graph.size()

    /**
     * A relation can be used as an extractor that matches the image of the
     * matched value `t`. E.g., the pattern `relation(a,b)` succeeds if and
     * only if the image of the matched value contains exactly two elements,
     * which are then bound to `a` and `b`, respectively. Normal sequence
     * matching works, such as `case relation(a, _*)` to match if there is
     * at least one element in the image and bind the first element to `a`.
     */
    def unapplySeq(t : T) : Option[Vector[U]] =
        Some(apply(t))

}

/**
 * Support for binary relations.
 */
object Relation {

    /**
     * A single empty image that can be used for all lookups.
     */
    val emptyImage = Vector()

    /**
     * Make a relation from a sequence of pairs that describe the mappings.
     */
    def fromPairs[T, U](pairs : Vector[(T, U)]) : Relation[T, U] = {
        val relation = new Relation[T, U]
        for ((t, u) <- pairs)
            relation.put(t, u)
        relation
    }

}
