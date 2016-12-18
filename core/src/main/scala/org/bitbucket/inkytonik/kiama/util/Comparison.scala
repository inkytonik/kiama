/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2016 Anthony M Sloane, Macquarie University.
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
package util

/**
 * Utility module for comparison routines.
 */
object Comparison {

    import scala.collection.mutable.TreeSet

    /**
     * Compare two arbitrary values. If they are both references, use
     * reference equality, otherwise use value equality.
     */
    def same(v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (d1 : Double, d2 : Double) =>
                    d1 == d2
                case (f1 : Float, f2 : Float) =>
                    f1 == f2
                case (i1 : Int, i2 : Int) =>
                    i1 == i2
                case (l1 : Long, l2 : Long) =>
                    l1 == l2
                case (r1 : AnyRef, r2 : AnyRef) =>
                    r1 eq r2
                case _ =>
                    sys.error(s"same: comparison of $v1 and $v2, should not be reached")
            }

    /**
     * An ordering that says two values are equal if `same` says they
     * are, otherwise earlier elements are greater than later ones.
     */
    class TOrdering[T] extends Ordering[T] {
        def compare(a : T, b : T) : Int =
            if (same(a, b)) 0 else 1
    }

    /**
     * Compare two `Iterable` collections or options and tuples containing that kind of
     * collection. Use `same` to compare the individual elements in the same order.
     */
    def sameCollection(v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (Some(s1), Some(s2)) =>
                    sameCollection(s1, s2)
                case ((t1, t2), (t3, t4)) =>
                    sameCollection(t1, t3) && sameCollection(t2, t4)
                case (t1 : Iterable[_], t2 : Iterable[_]) =>
                    (t1.size == t2.size) && (t1.zip(t2).forall(Function.tupled(sameCollection)))
                case _ =>
                    same(v1, v2)
            }

    /**
     * Compare two `Seq` collections or options and tuples containing that kind of
     * collection. Use `same` to compare the individual elements in any order.
     */
    def sameElements[T](t1 : Seq[_], t2 : Seq[_]) : Boolean =
        (t1.size == t2.size) && (t1.forall(contains(t2, _)))

    /**
     * As for `same`, except that if the two values are `Some` options
     * containing references, they are unwrapped first and the contents are
     * compared by reference.
     */
    def optsame(v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (Some(r1 : AnyRef), Some(r2 : AnyRef)) =>
                    r1 eq r2
                case _ =>
                    same(v1, v2)
            }

    /**
     * Does the sequence `s` contain `t`? Equality is tested using `same`.
     */
    def contains[T](s : Seq[T], t : T) : Boolean =
        s.exists(same(_, t))

    /**
     * Return a vector with only the distinct elements from the sequence `s`.
     * "distinct" in this case means compare using `same`.
     */
    def distinct[T](s : Seq[T]) : Vector[T] = {
        val set = new TreeSet[T]()(new TOrdering[T])
        set ++= s
        set.toVector
    }

    /**
     * As for `distinct` but works over a sequence of sequences.
     */
    def flatDistinct[T](ss : Seq[Seq[T]]) : Vector[T] = {
        val set = new TreeSet[T]()(new TOrdering[T])
        for (s <- ss)
            set ++= s
        set.toVector
    }

    /**
     * Return the zero-based index at which `elem` occurs in `s` using `same`
     * to perform comparisons, or -1 if `elem` does not occur in `s`.
     */
    def indexOf[T](s : Seq[T], elem : T) : Int =
        s.indexWhere(same(_, elem))

}
