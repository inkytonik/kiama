/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2016 Anthony M Sloane, Macquarie University.
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

import org.bitbucket.inkytonik.kiama.util.Memoiser.IdMemoised

/**
 * Representations of the graphs of relations between values of type `T` and `U`
 * backed by an identity cache.
 */
class RelationGraph[T, U] {

    import org.bitbucket.inkytonik.kiama.relation.Relation.{graphFromImages, graphFromPairs}
    import org.bitbucket.inkytonik.kiama.util.Comparison.flatDistinct

    /**
     * Backing memo table.
     */
    val memo = new IdMemoised[T, Vector[U]] {}

    /**
     * The domain of this graph.
     */
    lazy val domain : Vector[T] =
        memo.keys

    /**
     * Does the domain of this graph contain `t`?
     */
    def domainContains(t : T) : Boolean =
        memo.hasBeenComputedAt(t)

    /**
     * The image of this graph at domain value `t`.
     */
    def image(t : T) : Vector[U] =
        memo.getWithDefault(t, Vector())

    /**
     * All of the images of this graph.
     */
    lazy val images : Vector[Vector[U]] =
        memo.values

    /**
     * A graph that is the inverse of this one. That is, if `u` is in the image of `t`
     * in this graph then `t` will be in the image of `u` in the returned graph.
     */
    lazy val inverse : RelationGraph[U, T] =
        graphFromPairs(pairs.map(_.swap))

    /**
     * A graph that is the result of applying `f` to the images of domain values
     * in the current graph.
     */
    def mapValues[V](f : Vector[U] => Vector[V]) : RelationGraph[T, V] =
        graphFromImages(
            for (t <- domain) yield (t, f(image(t)))
        )

    /**
     * Get the individual pairs of this graph.
     */
    lazy val pairs : Vector[(T, U)] =
        for (t <- domain; u <- image(t))
            yield (t, u)

    /**
     * Put a new pair in this graph.
     */
    def put(t : T, u : U) {
        memo.updateAt(t, _ :+ u, Vector(u))
    }

    /**
     * Put new pairs in this graph.
     */
    def putAll(t : T, us : Vector[U]) {
        memo.updateAt(t, _ ++ us, us)
    }

    /**
     * The range of this graph.
     */
    lazy val range : Vector[U] =
        flatDistinct(memo.values)

    /**
     * The number of relationships in this graph.
     */
    lazy val size : Long =
        memo.size()

}
