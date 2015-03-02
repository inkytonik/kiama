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
package util

/**
 * Utility module for comparison routines.
 */
object Comparison {

    import scala.collection.immutable.Seq

    /**
     * Compare two arbitrary values. If they are both references, use
     * reference equality, otherwise use value equality.
     */
    def same (v1 : Any, v2 : Any) : Boolean =
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
                case (r1 : AnyRef, r2: AnyRef) =>
                    r1 eq r2
                case _ =>
                    sys.error (s"same: comparison of $v1 and $v2, should not be reached")
            }

    /**
     * Compare two `Iterable` collections or options and tuples containing that kind of
     * collection. Use `same` to compare the individual elements.
     */
    def samecollection (v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (Some (s1), Some (s2)) =>
                    samecollection (s1, s2)
                case ((t1,t2), (t3,t4)) =>
                    samecollection (t1, t3) && samecollection (t2, t4)
                case (t1 : Iterable[_], t2 : Iterable[_]) =>
                    (t1.size == t2.size) && (t1.zip (t2).forall (Function.tupled (samecollection)))
                case _ =>
                    same (v1, v2)
            }

    /**
     * As for `same`, except that if the two values are `Some` options
     * containing references, they are unwrapped first and the contents are
     * compared by reference.
     */
    def optsame (v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (Some (r1 : AnyRef), Some (r2 : AnyRef)) =>
                    r1 eq r2
                case _ =>
                    same (v1, v2)
            }

    /**
     * Does the finite sequence `s` contain `t`? Equality is tested using `same`.
     */
    def contains[T] (s : Seq[T], t : T) : Boolean =
        s.exists (same (_, t))

    /**
     * Return a list with only the distinct elements from the list `s`.
     * "distinct" in this case means compare unequal using `same`. The
     * first occurrence of each distinct element is kept.
     */
    def distinct[T] (s : List[T]) : List[T] = {

        import scala.collection.mutable.TreeSet

        /*
         * An ordering that says two elements are equal if `same` says they
         * are, otherwise uses the index (i.e., the position in `s`) so that
         * earlier elements are less than later ones.
         */
        object TOrdering extends Ordering[(T,Int)] {

              def compare (a : (T, Int), b : (T, Int)) : Int =
                  if (same (a._1, b._1)) 0 else a._2 - b._2

        }

        val set = new TreeSet[(T,Int)] () (TOrdering)
        set ++= (s.zipWithIndex)
        set.toList.map (_._1)

    }

}
