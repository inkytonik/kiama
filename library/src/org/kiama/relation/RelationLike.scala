/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2015 Anthony M Sloane, Macquarie University.
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
package relation

import scala.language.higherKinds

/**
 * A template trait for Relation-like types. `T` and `U` are the domain
 * and range types of the relation, respectively. `Repr` is the type
 * constructor for the concrete representation of a particular relation
 * type.
 */
trait RelationLike[T,U,Repr[_,_]] {

    import org.kiama.util.Comparison.{contains, distinct, same}
    import scala.collection.immutable.Seq

    /**
     * A companion object that provides factory methods for this kind of
     * relation.
     */
    def companion : RelationFactory[Repr]

    /**
     * The graph of this relation.
     */
    def graph : Seq[(T,U)]

    /**
     * Apply this relation (same as `image`).
     */
    def apply (t : T) : Seq[U] =
        image (t)

    /**
     * Build a new relation by collecting pairs produced by the partial
     * function `f` wherever it is defined on pairs of this relation.
     */
    def collect[V,W] (f : ((T,U)) ==> (V,W)) : Repr[V,W] =
        companion.fromGraph (graph.collect (f))

    /**
     * Compose this relation with `st`.
     */
    def compose[S] (st : RelationLike[S,T,Repr]) : Repr[S,U] =
        companion.fromGraph (
            for ((s, t1) <- st.graph; (t2, u) <- graph; if same (t1, t2))
                yield (s, u)
        )

    /**
     * Does the domain of this relation contain the value `t`?
     */
    def containsInDomain (t : T) : Boolean =
        contains (domain, t)

    /**
     * Does the range of this relation contain the value `u`?
     */
    def containsInRange (u : U) : Boolean =
        contains (range, u)

    /**
     * The domain of this relation.
     */
    lazy val domain : Seq[T] =
        distinct (graph.map (_._1))

    /**
     * The image of a value of the relation's domain is a set of the
     * values in the range that are related to that domain value.
     */
    def image (t : T) : Seq[U] =
        graph.collect { case (t1, u) if same (t, t1) => u }

    /**
     * A relation that maps each element of the range to its position
     * (starting counting at zero).
     */
    lazy val index : Repr[U,Int] =
        companion.fromGraph (graph.map (_._2).zipWithIndex)

    /**
     * Invert this relation. In other words, if `(t,u)` is in the relation,
     * then `(u,t)` is in the inverted relation.
     */
    lazy val inverse : Repr[U,T] =
        companion.fromGraph (graph.map (_.swap))

    /**
     * Is this relation empty (i.e., contains no pairs)?
     */
    lazy val isEmpty : Boolean =
        graph.isEmpty

    /**
     * An auxiliary extractor for this relation that matches pairs. The
     * match succeeds if and only if the matched value `t` has a unique
     * image in the relation. Both `t` and its unique image value are
     * returned for a successful match.
     */
    object pair {

        def unapply (t : T) : Option[(T,U)] =
            image (t) match {
                case Seq (u) => Some ((t, u))
                case _       => None
            }

    }

    /**
     * The preImage of a value of the relation's range is a set of the
     * values in the domain that are related to that range value.
     */
    def preImage (u : U) : Seq[T] =
        graph.collect { case (t, u1) if same (u, u1) => t }

    /**
     * A relation that maps each element of the domain to its position
     * starting at zero.
     */
    lazy val preIndex : Repr[T,Int] =
        companion.fromGraph (graph.map (_._1).zipWithIndex)

    /**
     * Domain projection, i.e., form a relation that relates each
     * value in the domain to all of the related values in the range.
     */
    lazy val projDomain : Repr[T,Seq[U]] =
        companion.fromGraph (domain.map (t => (t, image (t))))

    /**
     * Range projection, i.e., form a relation that relates each
     * value in the range to all of the related values in the domain.
     */
    lazy val projRange : Repr[U,Seq[T]] =
        companion.fromGraph (range.map (u => (u, preImage (u))))

    /**
     * The range of this relation.
     */
    lazy val range : Seq[U] =
        distinct (graph.map (_._2))

    /**
     * A relation can be used as an extractor that matches if and only if
     * the matched value `t` has a unique image in the relation. The unique
     * image value is returned for a successful match.
     */
    def unapply (t : T) : Option[U] =
        image (t) match {
            case Seq (u) => Some (u)
            case _       => None
        }

    /**
     * A relation can be used as an extractor that returns the image for a
     * given domain value `t`. Fails if `t` is not in the domain.
     */
    def unapplySeq (t : T) : Option[Seq[U]] = {
        val ti  = image (t)
        if (ti.isEmpty)
            None
        else
            Some (ti)
    }

    /**
     * Union this relation with `r`.
     */
    def union (r : RelationLike[T,U,Repr]) : Repr[T,U] =
        companion.fromGraph (graph ++ r.graph)

    /**
     * Return the sub-relation of this relation that contains just those
     * pairs that have `t` has their domain element.
     */
    def withDomain (t : T) : Repr[T,U] =
        companion.fromGraph (graph.filter { case (t1, _) => same (t, t1) })

    /**
     * Return the sub-relation of this relation that contains just those
     * pairs that have `u` has their range element.
     */
    def withRange (u : U) : Repr[T,U] =
        companion.fromGraph (graph.filter { case (_, u1) => same (u, u1) })

}
