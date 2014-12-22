/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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

import org.kiama.util.Tests
import scala.collection.immutable.Seq

/**
 * Tests of binary relations.
 */
class RelationTests extends Tests with RelationTestSupport {

    import org.scalacheck.Prop._
    import org.kiama.example.imperative.ImperativeTree.Num

    // Empty relations

    val emptyIntBool = new Relation[Int,Boolean] (Seq ())
    val emptyNumInt  = new Relation[Num,Int] (Seq ())
    val emptyBoolNum = new Relation[Boolean,Num] (Seq ())
    val emptyNumNum  = new Relation[Num,Num] (Seq ())

    // Singleton relations

    val num2 = Num (2)
    val num3 = Num (2)   // deliberately == to num2, but not same
    val num4 = Num (4)
    val num5 = Num (5)

    val singleIntBool = new Relation[Int,Boolean] (Seq ((1, true)))
    val singleNumInt  = new Relation[Num,Int] (Seq ((num2, 2)))
    val singleBoolNum = new Relation[Boolean,Num] (Seq ((false, num3)))
    val singleNumNum  = new Relation[Num,Num] (Seq ((num4, num5)))

    // Multiple element relations

    val multiIntBool = new Relation[Int,Boolean] (Seq ((1, true), (2, false), (1, true)))
    val multiNumInt  = new Relation[Num,Int] (Seq ((num2, 2), (num3, 3)))
    val multiBoolNum = new Relation[Boolean,Num] (Seq ((false, num3), (false, num4), (true, num4)))
    val multiNumNum  = new Relation[Num,Num] (Seq ((num4, num5), (num4, num5)))

    // collect

    test ("collect on an empty relation produces an empty relation") {
        assert (emptyBoolNum.collect { case (b, n) => (!b, n) }.isEmpty)
    }

    test ("collect same type on an singleton relation produces correct singleton relation") {
        assertResult (Seq ((true, num3))) (singleBoolNum.collect { case (b, n) => (!b, n) }.graph)
    }

    test ("collect new type on an singleton relation produces correct singleton relation") {
        assertResult (Seq ((num3, true))) (singleBoolNum.collect { case (b, n) => (n, !b) }.graph)
    }

    test ("collect same type on a multiple relation produces correct multiple relation (same Nums)") {
        assertResult (Seq ((num2, 3), (num3, 4))) (multiNumInt.collect { case (n, i) => (n, i + 1) }.graph)
    }

    test ("collect same type on a multiple relation produces correct multiple relation (new Nums)") {
        assertResult (Seq ((Num (3), 3), (Num (3), 4))) (multiNumInt.collect { case (Num (i), j) => (Num (i + 1), j + 1) }.graph)
    }

    test ("collect new type on a multiple relation produces correct multiple relation") {
        assertResult (Seq ((false, "bob"), (true, "bob"), (false, "bob"))) (multiIntBool.collect { case (i, b) => (!b, "bob") }.graph)
    }

    // compose

    test ("compose two empty relations produces an empty relation ") {
        assert (emptyBoolNum.compose (emptyIntBool).isEmpty)
    }

    test ("compose an empty relation on left of singleton relation produces an empty relation ") {
        assert (emptyIntBool.compose (singleNumInt).isEmpty)
    }

    test ("compose an empty relation on right of singleton relation produces an empty relation ") {
        assert (singleBoolNum.compose (emptyIntBool).isEmpty)
    }

    test ("compose a singleton relation with a mu;tiple relation produces the correct pairs") {
        assertResult (Seq ((num2, false))) (multiIntBool.compose (singleNumInt).graph)
    }

    test ("compose of two multiple relations produces the correct pairs") {
        assertResult (Seq ((1, num4), (2, num3), (2, num4), (1, num4))) (multiBoolNum.compose (multiIntBool).graph)
    }

    // containsInDomain

    test ("domain of singleton value-value relation contains its element") {
        assertResult (true) (singleIntBool.containsInDomain (1))
    }

    test ("domain of singleton value-value relation doesn't contain a non-element") {
        assertResult (false) (singleIntBool.containsInDomain (2))
    }

    test ("domain of singleton ref-value relation contains its element") {
        assertSameCollection (true) (singleNumInt.containsInDomain (num2))
    }

    test ("domain of singleton ref-value relation doesn't contain a non-element") {
        assertSameCollection (false) (singleNumInt.containsInDomain (num3))
    }

    test ("domain of singleton value-ref relation contains its element") {
        assertResult (true) (singleBoolNum.containsInDomain (false))
    }

    test ("domain of singleton value-ref relation doesn't contain a non-element") {
        assertResult (false) (singleBoolNum.containsInDomain (true))
    }

    test ("domain of singleton ref-ref relation contains its element") {
        assertSameCollection (true) (singleNumNum.containsInDomain (num4))
    }

    test ("domain of singleton ref-ref relation doesn't contain a non-element") {
        assertSameCollection (false) (singleNumNum.containsInDomain (num5))
    }

    test ("domain of multiple element value-value relation contains its first element") {
        assertResult (true) (multiIntBool.containsInDomain (1))
    }

    test ("domain of multiple element value-value relation contains its second element") {
        assertResult (true) (multiIntBool.containsInDomain (2))
    }

    test ("domain of multiple element value-value relation doesn't contain a non-element") {
        assertResult (false) (multiIntBool.containsInDomain (3))
    }

    test ("domain of multiple element ref-value relation contains its first element") {
        assertSameCollection (true) (multiNumInt.containsInDomain (num2))
    }

    test ("domain of multiple element ref-value relation contains its second element") {
        assertSameCollection (true) (multiNumInt.containsInDomain (num3))
    }

    test ("domain of multiple element ref-value relation doesn't contain a non-element") {
        assertSameCollection (false) (multiNumInt.containsInDomain (num4))
    }

    test ("domain of multiple element value-ref relation contains its first element") {
        assertResult (true) (multiBoolNum.containsInDomain (false))
    }

    test ("domain of multiple element value-ref relation contains its second element") {
        assertResult (true) (multiBoolNum.containsInDomain (true))
    }

    test ("domain of multiple element ref-ref relation contains its element") {
        assertSameCollection (true) (multiNumNum.containsInDomain (num4))
    }

    test ("domain of multiple element ref-ref relation doesn't contain a non-element") {
        assertSameCollection (false) (multiNumNum.containsInDomain (num5))
    }

    // containsInRange

    test ("range of singleton value-value relation contains its element") {
        assertResult (true) (singleIntBool.containsInRange (true))
    }

    test ("range of singleton value-value relation doesn't contain a non-element") {
        assertResult (false) (singleIntBool.containsInRange (false))
    }

    test ("range of singleton ref-value relation contains its element") {
        assertResult (true) (singleNumInt.containsInRange (2))
    }

    test ("range of singleton ref-value relation doesn't contain a non-element") {
        assertResult (false) (singleNumInt.containsInRange (3))
    }

    test ("range of singleton value-ref relation contains its element") {
        assertSameCollection (true) (singleBoolNum.containsInRange (num3))
    }

    test ("range of singleton value-ref relation doesn't contain a non-element") {
        assertSameCollection (false) (singleBoolNum.containsInRange (num2))
    }

    test ("range of singleton ref-ref relation contains its element") {
        assertSameCollection (true) (singleNumNum.containsInRange (num5))
    }

    test ("range of singleton ref-ref relation doesn't contain a non-element") {
        assertSameCollection (false) (singleNumNum.containsInRange (num4))
    }

    test ("range of multiple element value-value relation contains its first element") {
        assertResult (true) (multiIntBool.containsInRange (true))
    }

    test ("range of multiple element value-value relation contains its second element") {
        assertResult (true) (multiIntBool.containsInRange (false))
    }

    test ("range of multiple element ref-value relation contains its first element") {
        assertResult (true) (multiNumInt.containsInRange (2))
    }

    test ("range of multiple element ref-value relation contains its second element") {
        assertResult (true) (multiNumInt.containsInRange (3))
    }

    test ("range of multiple element ref-value relation doesn't contain a non-element") {
        assertResult (false) (multiNumInt.containsInRange (4))
    }

    test ("range of multiple element value-ref relation contains its first element") {
        assertSameCollection (true) (multiBoolNum.containsInRange (num3))
    }

    test ("range of multiple element value-ref relation contains its second element") {
        assertSameCollection (true) (multiBoolNum.containsInRange (num4))
    }

    test ("range of multiple element value-ref relation doesn't contain a non-element") {
        assertSameCollection (false) (multiBoolNum.containsInRange (num2))
    }

    test ("range of multiple element ref-ref relation contains its element") {
        assertSameCollection (true) (multiNumNum.containsInRange (num5))
    }

    test ("range of multiple element ref-ref relation doesn't contain a non-element") {
        assertSameCollection (false) (multiNumNum.containsInRange (num4))
    }

    // domain

    test ("domain of empty value-value relation is empty") {
        assert (emptyIntBool.domain.isEmpty)
    }

    test ("domain of empty ref-value relation is empty") {
        assert (emptyNumInt.domain.isEmpty)
    }

    test ("domain of empty value-ref relation is empty") {
        assert (emptyBoolNum.domain.isEmpty)
    }

    test ("domain of empty ref-ref relation is empty") {
        assert (emptyNumNum.domain.isEmpty)
    }

    test ("domain of singleton value-value relation is correct") {
        assertResult (Seq (1)) (singleIntBool.domain)
    }

    test ("domain of singleton ref-value relation is correct") {
        assertSameCollection (Seq (num2)) (singleNumInt.domain)
    }

    test ("domain of singleton value-ref relation is correct") {
        assertResult (Seq (false)) (singleBoolNum.domain)
    }

    test ("domain of singleton ref-ref relation is correct") {
        assertSameCollection (Seq (num4)) (singleNumNum.domain)
    }

    test ("domain of multiple element value-value relation is correct") {
        assertResult (Seq (1, 2)) (multiIntBool.domain)
    }

    test ("domain of multiple element ref-value relation is correct") {
        assertSameCollection (Seq (num2, num3)) (multiNumInt.domain)
    }

    test ("domain of multiple element value-ref relation is correct") {
        assertResult (Seq (false, true)) (multiBoolNum.domain)
    }

    test ("domain of multiple element ref-ref relation is correct") {
        assertSameCollection (Seq (num4)) (multiNumNum.domain)
    }

    // image

    test ("image of empty value-value relation is empty") {
        assert (emptyIntBool.image (1).isEmpty)
    }

    test ("image of empty ref-value relation is empty") {
        assert (emptyNumInt.image (num2).isEmpty)
    }

    test ("image of empty value-ref relation is empty") {
        assert (emptyBoolNum.image (false).isEmpty)
    }

    test ("image of empty ref-ref relation is empty") {
        assert (emptyNumNum.image (num3).isEmpty)
    }

    test ("image of singleton value-value relation is correct (present)") {
        assertResult (Seq (true)) (singleIntBool.image (1))
    }

    test ("image of singleton value-value relation is empty (not present)") {
        assert (singleIntBool.image (2).isEmpty)
    }

    test ("image of singleton ref-value relation is correct (present)") {
        assertResult (Seq (2)) (singleNumInt.image (num2))
    }

    test ("image of singleton ref-value relation is empty (not present)") {
        assert (singleNumInt.image (num3).isEmpty)
    }

    test ("image of singleton value-ref relation is correct (present)") {
        assertSameCollection (Seq (num3)) (singleBoolNum.image (false))
    }

    test ("image of singleton value-ref relation is empty (not present)") {
        assert (singleBoolNum.image (true).isEmpty)
    }

    test ("image of singleton ref-ref relation is correct (present)") {
        assertSameCollection (Seq (num5)) (singleNumNum.image (num4))
    }

    test ("image of singleton ref-ref relation is empty (not present)") {
        assert (singleNumNum.image (num5).isEmpty)
    }

    test ("image of multiple element value-value relation is correct (present 1)") {
        assertResult (Seq (true, true)) (multiIntBool.image (1))
    }

    test ("image of multiple element value-value relation is correct (present 2)") {
        assertResult (Seq (false)) (multiIntBool.image (2))
    }

    test ("image of multiple element value-value relation is empty (not present)") {
        assert (multiIntBool.image (3).isEmpty)
    }

    test ("image of multiple element ref-value relation is correct (present)") {
        assertResult (Seq (2)) (multiNumInt.image (num2))
    }

    test ("image of multiple element ref-value relation is empty (not present)") {
        assert (multiNumInt.image (num4).isEmpty)
    }

    test ("image of multiple element value-ref relation is correct (present 1)") {
        assertSameCollection (Seq (num3, num4)) (multiBoolNum.image (false))
    }

    test ("image of multiple element value-ref relation is correct (present 2)") {
        assertSameCollection (Seq (num4)) (multiBoolNum.image (true))
    }

    test ("image of multiple element ref-ref relation is correct (present)") {
        assertSameCollection (Seq (num5, num5)) (multiNumNum.image (num4))
    }

    test ("image of multiple element ref-ref relation is empty (not present)") {
        assert (multiNumNum.image (num2).isEmpty)
    }

    // index

    test ("index of empty value-value relation is empty") {
        assert (emptyIntBool.index.isEmpty)
    }

    test ("index of empty ref-value relation is empty") {
        assert (emptyNumInt.index.isEmpty)
    }

    test ("index of empty value-ref relation is empty") {
        assert (emptyBoolNum.index.isEmpty)
    }

    test ("index of empty ref-ref relation is empty") {
        assert (emptyNumNum.index.isEmpty)
    }

    test ("index of singleton value-value relation is correct") {
        assertResult (Seq ((true, 0))) (singleIntBool.index.graph)
    }

    test ("index of singleton ref-value relation is correct") {
        assertResult (Seq ((2, 0))) (singleNumInt.index.graph)
    }

    test ("index of singleton value-ref relation is correct") {
        assertSameCollection (Seq ((num3, 0))) (singleBoolNum.index.graph)
    }

    test ("index of singleton ref-ref relation is correct (present)") {
        assertSameCollection (Seq ((num5, 0))) (singleNumNum.index.graph)
    }

    test ("index of multiple element value-value relation is correct") {
        assertResult (Seq ((true, 0), (false, 1), (true, 2))) (multiIntBool.index.graph)
    }

    test ("index of multiple element ref-value relation is correct") {
        assertResult (Seq ((2, 0), (3, 1))) (multiNumInt.index.graph)
    }

    test ("index of multiple element value-ref relation is correct") {
        assertSameCollection (Seq ((num3, 0), (num4, 1), (num4, 2))) (multiBoolNum.index.graph)
    }

    test ("index of multiple element ref-ref relation is correct") {
        assertSameCollection (Seq ((num5, 0), (num5, 1))) (multiNumNum.index.graph)
    }

    // inverse

    test ("inverting an empty relation yields an empty relation") {
        assert (emptyIntBool.inverse.isEmpty)
    }

    test ("inverting a singleton relation yields the correct singleton relation") {
        assertResult (Seq ((true, 1))) (singleIntBool.inverse.graph)
    }

    test ("inverting a multiple relation yields the correct multiple relation") {
        assertResult (Seq ((num3, false), (num4, false), (num4, true))) (multiBoolNum.inverse.graph)
    }

    // preImage

    test ("preImage of empty value-value relation is empty") {
        assert (emptyIntBool.preImage (false).isEmpty)
    }

    test ("preImage of empty ref-value relation is empty") {
        assert (emptyNumInt.preImage (2).isEmpty)
    }

    test ("preImage of empty value-ref relation is empty") {
        assert (emptyBoolNum.preImage (num2).isEmpty)
    }

    test ("preImage of empty ref-ref relation is empty") {
        assert (emptyNumNum.preImage (num3).isEmpty)
    }

    test ("preImage of singleton value-value relation is correct (present)") {
        assertResult (Seq (1)) (singleIntBool.preImage (true))
    }

    test ("preImage of singleton value-value relation is empty (not present)") {
        assert (singleIntBool.preImage (false).isEmpty)
    }

    test ("preImage of singleton ref-value relation is correct (present)") {
        assertSameCollection (Seq (num2)) (singleNumInt.preImage (2))
    }

    test ("preImage of singleton ref-value relation is empty (not present)") {
        assert (singleNumInt.preImage (3).isEmpty)
    }

    test ("preImage of singleton value-ref relation is correct (present)") {
        assertResult (Seq (false)) (singleBoolNum.preImage (num3))
    }

    test ("preImage of singleton value-ref relation is empty (not present)") {
        assert (singleBoolNum.preImage (num2).isEmpty)
    }

    test ("preImage of singleton ref-ref relation is correct (present)") {
        assertSameCollection (Seq (num4)) (singleNumNum.preImage (num5))
    }

    test ("preImage of singleton ref-ref relation is empty (not present)") {
        assert (singleNumNum.preImage (num4).isEmpty)
    }

    test ("preImage of multiple element value-value relation is correct (present 1)") {
        assertResult (Seq (1, 1)) (multiIntBool.preImage (true))
    }

    test ("preImage of multiple element value-value relation is correct (present 2)") {
        assertResult (Seq (2)) (multiIntBool.preImage (false))
    }

    test ("preImage of multiple element ref-value relation is correct (present)") {
        assertSameCollection (Seq (num2)) (multiNumInt.preImage (2))
    }

    test ("preImage of multiple element ref-value relation is empty (not present)") {
        assert (multiNumInt.preImage (4).isEmpty)
    }

    test ("preImage of multiple element value-ref relation is correct (present 1)") {
        assertResult (Seq (false)) (multiBoolNum.preImage (num3))
    }

    test ("preImage of multiple element value-ref relation is correct (present 2)") {
        assertResult (Seq (false, true)) (multiBoolNum.preImage (num4))
    }

    test ("preImage of multiple element value-ref relation is empty (not present)") {
        assert (multiBoolNum.preImage (num2).isEmpty)
    }

    test ("preImage of multiple element ref-ref relation is correct (present)") {
        assertSameCollection (Seq (num4, num4)) (multiNumNum.preImage (num5))
    }

    test ("preImage of multiple element ref-ref relation is empty (not present)") {
        assert (multiNumNum.preImage (num2).isEmpty)
    }

    // preIndex

    test ("preIndex of empty value-value relation is empty") {
        assert (emptyIntBool.preIndex.isEmpty)
    }

    test ("preIndex of empty ref-value relation is empty") {
        assert (emptyNumInt.preIndex.isEmpty)
    }

    test ("preIndex of empty value-ref relation is empty") {
        assert (emptyBoolNum.preIndex.isEmpty)
    }

    test ("preIndex of empty ref-ref relation is empty") {
        assert (emptyNumNum.preIndex.isEmpty)
    }

    test ("preIndex of singleton value-value relation is correct") {
        assertResult (Seq ((1, 0))) (singleIntBool.preIndex.graph)
    }

    test ("preIndex of singleton ref-value relation is correct") {
        assertSameCollection (Seq ((num2, 0))) (singleNumInt.preIndex.graph)
    }

    test ("preIndex of singleton value-ref relation is correct") {
        assertResult (Seq ((false, 0))) (singleBoolNum.preIndex.graph)
    }

    test ("preIndex of singleton ref-ref relation is correct (present)") {
        assertSameCollection (Seq ((num4, 0))) (singleNumNum.preIndex.graph)
    }

    test ("preIndex of multiple element value-value relation is correct") {
        assertResult (Seq ((1, 0), (2, 1), (1, 2))) (multiIntBool.preIndex.graph)
    }

    test ("preIndex of multiple element ref-value relation is correct") {
        assertSameCollection (Seq ((num2, 0), (num3, 1))) (multiNumInt.preIndex.graph)
    }

    test ("preIndex of multiple element value-ref relation is correct") {
        assertResult (Seq ((false, 0), (false, 1), (true, 2))) (multiBoolNum.preIndex.graph)
    }

    test ("preIndex of multiple element ref-ref relation is correct") {
        assertSameCollection (Seq ((num4, 0), (num4, 1))) (multiNumNum.preIndex.graph)
    }

    // projDomain

    test ("projDomain of empty value-value relation is empty") {
        assert (emptyIntBool.projDomain.isEmpty)
    }

    test ("projDomain of empty ref-value relation is empty") {
        assert (emptyNumInt.projDomain.isEmpty)
    }

    test ("projDomain of empty value-ref relation is empty") {
        assert (emptyBoolNum.projDomain.isEmpty)
    }

    test ("projDomain of empty ref-ref relation is empty") {
        assert (emptyNumNum.projDomain.isEmpty)
    }

    test ("projDomain of singleton value-value relation is correct") {
        assertResult (Seq ((1, Seq (true)))) (singleIntBool.projDomain.graph)
    }

    test ("projDomain of singleton ref-value relation is correct") {
        assertSameCollection (Seq ((num2, Seq (2)))) (singleNumInt.projDomain.graph)
    }

    test ("projDomain of singleton value-ref relation is correct") {
        assertSameCollection (Seq ((false, Seq (num3)))) (singleBoolNum.projDomain.graph)
    }

    test ("projDomain of singleton ref-ref relation is correct") {
        assertSameCollection (Seq ((num4, Seq (num5)))) (singleNumNum.projDomain.graph)
    }

    test ("projDomain of multiple element value-value relation is correct") {
        assertResult (Seq ((1, Seq (true, true)), (2, Seq (false)))) (multiIntBool.projDomain.graph)
    }

    test ("projDomain of multiple element ref-value relation is correct") {
        assertSameCollection (Seq ((num2, Seq (2)), (num3, Seq (3)))) (multiNumInt.projDomain.graph)
    }

    test ("projDomain of multiple element value-ref relation is correct") {
        assertSameCollection (Seq ((false, Seq (num3, num4)), (true, Seq (num4)))) (multiBoolNum.projDomain.graph)
    }

    test ("projDomain of multiple element ref-ref relation is correct") {
        assertSameCollection (Seq ((num4, Seq (num5, num5)))) (multiNumNum.projDomain.graph)
    }

    // projRange

    test ("projRange of empty value-value relation is empty") {
        assert (emptyIntBool.projRange.isEmpty)
    }

    test ("projRange of empty ref-value relation is empty") {
        assert (emptyNumInt.projRange.isEmpty)
    }

    test ("projRange of empty value-ref relation is empty") {
        assert (emptyBoolNum.projRange.isEmpty)
    }

    test ("projRange of empty ref-ref relation is empty") {
        assert (emptyNumNum.projRange.isEmpty)
    }

    test ("projRange of singleton value-value relation is correct") {
        assertResult (Seq ((true, Seq (1)))) (singleIntBool.projRange.graph)
    }

    test ("projRange of singleton ref-value relation is correct") {
        assertSameCollection (Seq ((2, Seq (num2)))) (singleNumInt.projRange.graph)
    }

    test ("projRange of singleton value-ref relation is correct") {
        assertSameCollection (Seq ((num3, Seq (false)))) (singleBoolNum.projRange.graph)
    }

    test ("projRange of singleton ref-ref relation is correct") {
        assertSameCollection (Seq ((num5, Seq (num4)))) (singleNumNum.projRange.graph)
    }

    test ("projRange of multiple element value-value relation is correct") {
        assertResult (Seq ((true, Seq (1, 1)), (false, Seq (2)))) (multiIntBool.projRange.graph)
    }

    test ("projRange of multiple element ref-value relation is correct") {
        assertSameCollection (Seq ((2, Seq (num2)), (3, Seq (num3)))) (multiNumInt.projRange.graph)
    }

    test ("projRange of multiple element value-ref relation is correct") {
        assertSameCollection (Seq ((num3, Seq (false)), (num4, Seq (false, true)))) (multiBoolNum.projRange.graph)
    }

    test ("projRange of multiple element ref-ref relation is correct") {
        assertSameCollection (Seq ((num5, Seq (num4, num4)))) (multiNumNum.projRange.graph)
    }

    // range

    test ("range of empty value-value relation is empty") {
        assert (emptyIntBool.range.isEmpty)
    }

    test ("range of empty ref-value relation is empty") {
        assert (emptyNumInt.range.isEmpty)
    }

    test ("range of empty value-ref relation is empty") {
        assert (emptyBoolNum.range.isEmpty)
    }

    test ("range of empty ref-ref relation is empty") {
        assert (emptyNumNum.range.isEmpty)
    }

    test ("range of singleton value-value relation is correct") {
        assertResult (Seq (true)) (singleIntBool.range)
    }

    test ("range of singleton ref-value relation is correct") {
        assertResult (Seq (2)) (singleNumInt.range)
    }

    test ("range of singleton value-ref relation is correct") {
        assertSameCollection (Seq (num3)) (singleBoolNum.range)
    }

    test ("range of singleton ref-ref relation is correct") {
        assertSameCollection (Seq (num5)) (singleNumNum.range)
    }

    test ("range of multiple element value-value relation is correct") {
        assertResult (Seq (true, false)) (multiIntBool.range)
    }

    test ("range of multiple element ref-value relation is correct") {
        assertResult (Seq (2, 3)) (multiNumInt.range)
    }

    test ("range of multiple element value-ref relation is correct") {
        assertSameCollection (Seq (num3, num4)) (multiBoolNum.range)
    }

    test ("range of multiple element ref-ref relation is correct") {
        assertSameCollection (Seq (num5)) (multiNumNum.range)
    }

    // unapply

    test ("unapply of empty value-value relation fails") {
        assertResult (None) (emptyIntBool.unapply (1))
    }

    test ("unapply of empty ref-value relation fails") {
        assertResult (None) (emptyNumInt.unapply (num2))
    }

    test ("unapply of empty value-ref relation fails") {
        assertResult (None) (emptyBoolNum.unapply (false))
    }

    test ("unapply of empty ref-ref relation fails") {
        assertResult (None) (emptyNumNum.unapply (num3))
    }

    test ("unapply of singleton value-value relation is correct (present)") {
        assertResult (Some (true)) (singleIntBool.unapply (1))
    }

    test ("unapply pair of singleton value-value relation is correct (present)") {
        assertResult (Some ((1, true))) (singleIntBool.pair.unapply (1))
    }

    test ("unapply of singleton value-value relation fails (not present)") {
        assertResult (None) (singleIntBool.unapply (2))
    }

    test ("unapply of singleton ref-value relation is correct (present)") {
        assertResult (Option (2)) (singleNumInt.unapply (num2))
    }

    // This test makes sure that we are comparing nodes by identity since
    // num2 and num3 are equal by value.

    test ("unapply pair of singleton ref-value relation doesn't produce equal but not eq value (present)") {
        assertNotSameCollection (Option ((num3, 2))) (singleNumInt.pair.unapply (num2))
    }

    test ("unapply of singleton ref-value relation fails (not present)") {
        assertResult (None) (singleNumInt.unapply (num3))
    }

    test ("unapply of singleton value-ref relation is correct (present)") {
        assertSameCollection (Option (num3)) (singleBoolNum.unapply (false))
    }

    test ("unapply pair of singleton value-ref relation is correct (present)") {
        assertSameCollection (Option ((false, num3))) (singleBoolNum.pair.unapply (false))
    }

    test ("unapply of singleton value-ref relation fails (not present)") {
        assertResult (None) (singleBoolNum.unapply (true))
    }

    test ("unapply of singleton ref-ref relation is correct (present)") {
        assertSameCollection (Option (num5)) (singleNumNum.unapply (num4))
    }

    test ("unapply pair of singleton ref-ref relation is correct (present)") {
        assertSameCollection (Option ((num4, num5))) (singleNumNum.pair.unapply (num4))
    }

    test ("unapply of singleton ref-ref relation fails (not present)") {
        assertResult (None) (singleNumNum.unapply (num5))
    }

    test ("unapply of multiple element value-value relation fails (multiple)") {
        assertResult (None) (multiIntBool.unapply (1))
    }

    test ("unapply of multiple element value-value relation is correct (present)") {
        assertResult (Option (false)) (multiIntBool.unapply (2))
    }

    test ("unapply pair of multiple element value-value relation is correct (present)") {
        assertResult (Option ((2, false))) (multiIntBool.pair.unapply (2))
    }

    test ("unapply of multiple element value-value relation fails (not present)") {
        assertResult (None) (multiIntBool.unapply (3))
    }

    test ("unapply of multiple element ref-value relation is correct (present)") {
        assertResult (Option (2)) (multiNumInt.unapply (num2))
    }

    test ("unapply pair of multiple element ref-value relation is correct (present)") {
        assertSameCollection (Option ((num2, 2))) (multiNumInt.pair.unapply (num2))
    }

    test ("unapply of multiple element ref-value relation is correct (not present)") {
        assertResult (None) (multiNumInt.unapply (num4))
    }

    test ("unapply of multiple element value-ref relation fails (multiple)") {
        assertResult (None) (multiBoolNum.unapply (false))
    }

    test ("unapply of multiple element value-ref relation is correct (present)") {
        assertSameCollection (Option (num4)) (multiBoolNum.unapply (true))
    }

    test ("unapply pair of multiple element value-ref relation is correct (present)") {
        assertSameCollection (Option ((true, num4))) (multiBoolNum.pair.unapply (true))
    }

    test ("unapply of multiple element ref-ref relation is correct (multiple)") {
        assertResult (None) (multiNumNum.unapply (num4))
    }

    test ("unapply of multiple element ref-ref relation is correct (not present)") {
        assertResult (None) (multiNumNum.unapply (num2))
    }

    // unapplySeq

    test ("unapplySeq of an empty relation fails") {
        assertResult (None) (emptyIntBool.unapplySeq (1))
    }

    test ("unapplySeq of a singleton relation is correct (present)") {
        assertSameCollection (Option (Seq (2))) (singleNumInt.unapplySeq (num2))
    }

    test ("unapplySeq of a singleton relation fails (not present)") {
        assertResult (None) (singleNumInt.unapplySeq (num3))
    }

    test ("unapplySeq of a multiple relation is correct (present)") {
        assertSameCollection (Option (Seq (true, true))) (multiIntBool.unapplySeq (1))
    }

    test ("unapplySeq of a multiple relation fails (not present)") {
        assertResult (None) (multiIntBool.unapplySeq (3))
    }

    // union

    test ("an empty relation union an empty relation is empty (value-value)") {
        val r = new Relation[Int,Boolean] (Seq ())
        assertResult (true) (emptyIntBool.union (r).isEmpty)
    }

    test ("an empty relation union an empty relation is empty (ref-value)") {
        val r = new Relation[Num,Int] (Seq ())
        assertResult (true) (emptyNumInt.union (r).isEmpty)
    }

    test ("an empty relation union an empty relation is empty (value-ref)") {
        val r = new Relation[Boolean,Num] (Seq ())
        assertResult (true) (emptyBoolNum.union (r).isEmpty)
    }

    test ("an empty relation union an empty relation is empty (ref-ref)") {
        val r = new Relation[Num,Num] (Seq ())
        assertResult (true) (emptyNumNum.union (r).isEmpty)
    }

    test ("an empty relation union a non-empty relation has correct graph (value-value)") {
        assertResult (Seq ((1, true))) (emptyIntBool.union (singleIntBool).graph)
    }

    test ("a non-empty relation union an empty relation has correct graph (value-value)") {
        assertResult (Seq ((1, true))) (singleIntBool.union (emptyIntBool).graph)
    }

    test ("an empty relation union a non-empty relation has correct graph (ref-value)") {
        assertSameCollection (Seq ((num2, 2))) (emptyNumInt.union (singleNumInt).graph)
    }

    test ("a non-empty relation union an empty relation has correct graph (ref-value)") {
        assertSameCollection (Seq ((num2, 2))) (singleNumInt.union (emptyNumInt).graph)
    }

    test ("an empty relation union a non-empty relation has correct graph (value-ref)") {
        assertSameCollection (Seq ((false, num3))) (emptyBoolNum.union (singleBoolNum).graph)
    }

    test ("a non-empty relation union an empty relation has correct graph (value-ref)") {
        assertSameCollection (Seq ((false, num3))) (singleBoolNum.union (emptyBoolNum).graph)
    }

    test ("an empty relation union a non-empty relation has correct graph (ref-ref)") {
        assertSameCollection (Seq ((num4, num5))) (emptyNumNum.union (singleNumNum).graph)
    }

    test ("a non-empty relation union an empty relation has correct graph (ref-ref)") {
        assertSameCollection (Seq ((num4, num5))) (singleNumNum.union (emptyNumNum).graph)
    }

    test ("union of non-empty relations has correct graph (value-value)") {
        val r = new Relation[Int,Boolean] (Seq ((42, false), (99, true)))
        assertResult (Seq ((1, true), (2, false), (1, true), (42, false), (99, true))) (multiIntBool.union (r).graph)
    }

    test ("union of non-empty relations has correct graph (ref-value)") {
        val r = new Relation[Num,Int] (Seq ((num4, 42)))
        assertSameCollection (Seq ((num2, 2), (num3, 3), (num4, 42))) (multiNumInt.union (r).graph)
    }

    test ("union of non-empty relations has correct graph (value-ref)") {
        val r = new Relation[Boolean,Num] (Seq ((false, num3), (true, num2)))
        assertSameCollection (Seq ((false, num3), (false, num4), (true, num4), (false, num3), (true, num2))) (multiBoolNum.union (r).graph)
    }

    test ("union of non-empty relations has correct graph (ref-ref)") {
        val r = new Relation[Num,Num] (Seq ((num2, num3), (num2, num3)))
        assertSameCollection (Seq ((num4, num5), (num4, num5), (num2, num3), (num2, num3))) (multiNumNum.union (r).graph)
    }

    // withDomain

    test ("withDomain of empty value-value relation is an empty relation") {
        assertResult (true) (emptyIntBool.withDomain (1).isEmpty)
    }

    test ("withDomain of empty ref-value relation is an empty relation") {
        assertResult (true) (emptyNumInt.withDomain (num2).isEmpty)
    }

    test ("withDomain of empty value-ref relation is an empty relation") {
        assertResult (true) (emptyBoolNum.withDomain (true).isEmpty)
    }

    test ("withDomain of empty ref-ref relation is an empty relation") {
        assertResult (true) (emptyNumNum.withDomain (num4).isEmpty)
    }

    test ("withDomain of singleton value-value relation of element has correct domain") {
        assertResult (Seq (1)) (singleIntBool.withDomain (1).domain)
    }

    test ("withDomain of singleton value-value relation of element has correct range") {
        assertResult (Seq (true)) (singleIntBool.withDomain (1).range)
    }

    test ("withDomain of singleton value-value relation of non-element is empty") {
        assertResult (Seq ()) (singleIntBool.withDomain (2).domain)
    }

    test ("withDomain of singleton ref-value relation of element has correct domain") {
        assertSameCollection (Seq (num2)) (singleNumInt.withDomain (num2).domain)
    }

    test ("withDomain of singleton ref-value relation of element has correct range") {
        assertResult (Seq (2)) (singleNumInt.withDomain (num2).range)
    }

    test ("withDomain of singleton ref-value relation of non-element is empty") {
        assertResult (Seq ()) (singleNumInt.withDomain (num3).domain)
    }

    test ("withDomain of singleton value-ref relation of element has correct domain") {
        assertResult (Seq (false)) (singleBoolNum.withDomain (false).domain)
    }

    test ("withDomain of singleton value-ref relation of element has correct range") {
        assertSameCollection (Seq (num3)) (singleBoolNum.withDomain (false).range)
    }

    test ("withDomain of singleton value-ref relation of non-element is empty") {
        assertResult (Seq ()) (singleBoolNum.withDomain (true).domain)
    }

    test ("withDomain of singleton ref-ref relation of element has correct domain") {
        assertSameCollection (Seq (num4)) (singleNumNum.withDomain (num4).domain)
    }

    test ("withDomain of singleton ref-ref relation of element has correct range") {
        assertSameCollection (Seq (num5)) (singleNumNum.withDomain (num4).range)
    }

    test ("withDomain of singleton ref-ref relation of non-element is empty") {
        assertResult (Seq ()) (singleNumNum.withDomain (num5).domain)
    }

    test ("withDomain of multiple element value-value relation of first element has correct domain") {
        assertResult (Seq (1)) (multiIntBool.withDomain (1).domain)
    }

    test ("withDomain of multiple element value-value relation of second element has correct domain") {
        assertResult (Seq (2)) (multiIntBool.withDomain (2).domain)
    }

    test ("withDomain of multiple element value-value relation of first element has correct range") {
        assertResult (Seq (true)) (multiIntBool.withDomain (1).range)
    }

    test ("withDomain of multiple element value-value relation of second element has correct range") {
        assertResult (Seq (false)) (multiIntBool.withDomain (2).range)
    }

    test ("withDomain of multiple element value-value relation of non-element is empty") {
        assertResult (Seq ()) (multiIntBool.withDomain (3).domain)
    }

    test ("withDomain of multiple element ref-value relation of first element has correct domain") {
        assertSameCollection (Seq (num2)) (multiNumInt.withDomain (num2).domain)
    }

    test ("withDomain of multiple element ref-value relation of second element has correct domain") {
        assertSameCollection (Seq (num3)) (multiNumInt.withDomain (num3).domain)
    }

    test ("withDomain of multiple element ref-value relation of first element has correct range") {
        assertResult (Seq (2)) (multiNumInt.withDomain (num2).range)
    }

    test ("withDomain of multiple element ref-value relation of second element has correct range") {
        assertResult (Seq (3)) (multiNumInt.withDomain (num3).range)
    }

    test ("withDomain of multiple element ref-value relation of non-element is empty") {
        assertResult (Seq ()) (multiNumInt.withDomain (num4).domain)
    }

    test ("withDomain of multiple element value-ref relation of first element has correct domain") {
        assertResult (Seq (false)) (multiBoolNum.withDomain (false).domain)
    }

    test ("withDomain of multiple element value-ref relation of second element has correct domain") {
        assertResult (Seq (true)) (multiBoolNum.withDomain (true).domain)
    }

    test ("withDomain of multiple element value-ref relation of first element has correct range") {
        assertSameCollection (Seq (num3, num4)) (multiBoolNum.withDomain (false).range)
    }

    test ("withDomain of multiple element value-ref relation of second element has correct range") {
        assertSameCollection (Seq (num4)) (multiBoolNum.withDomain (true).range)
    }

    test ("withDomain of multiple element ref-ref relation of element has correct domain") {
        assertSameCollection (Seq (num4)) (multiNumNum.withDomain (num4).domain)
    }

    test ("withDomain of multiple element ref-ref relation of element has correct range") {
        assertSameCollection (Seq (num5)) (multiNumNum.withDomain (num4).range)
    }

    test ("withDomain of multiple element ref-ref relation of non-element is empty") {
        assertSameCollection (Seq ()) (multiNumNum.withDomain (num5).domain)
    }

    // withRange

    test ("withRange of empty value-value relation is an empty relation") {
        assertResult (true) (emptyIntBool.withRange (true).isEmpty)
    }

    test ("withRange of empty ref-value relation is an empty relation") {
        assertResult (true) (emptyNumInt.withRange (2).isEmpty)
    }

    test ("withRange of empty value-ref relation is an empty relation") {
        assertResult (true) (emptyBoolNum.withRange (num2).isEmpty)
    }

    test ("withRange of empty ref-ref relation is an empty relation") {
        assertResult (true) (emptyNumNum.withRange (num4).isEmpty)
    }

    test ("withRange of singleton value-value relation of element has correct domain") {
        assertResult (Seq (1)) (singleIntBool.withRange (true).domain)
    }

    test ("withRange of singleton value-value relation of element has correct range") {
        assertResult (Seq (true)) (singleIntBool.withRange (true).range)
    }

    test ("withRange of singleton value-value relation of non-element is empty") {
        assertResult (Seq ()) (singleIntBool.withRange (false).domain)
    }

    test ("withRange of singleton ref-value relation of element has correct domain") {
        assertSameCollection (Seq (num2)) (singleNumInt.withRange (2).domain)
    }

    test ("withRange of singleton ref-value relation of element has correct range") {
        assertResult (Seq (2)) (singleNumInt.withRange (2).range)
    }

    test ("withRange of singleton ref-value relation of non-element is empty") {
        assertResult (Seq ()) (singleNumInt.withRange (3).domain)
    }

    test ("withRange of singleton value-ref relation of element has correct domain") {
        assertResult (Seq (false)) (singleBoolNum.withRange (num3).domain)
    }

    test ("withRange of singleton value-ref relation of element has correct range") {
        assertSameCollection (Seq (num3)) (singleBoolNum.withRange (num3).range)
    }

    test ("withRange of singleton value-ref relation of non-element is empty") {
        assertResult (Seq ()) (singleBoolNum.withRange (num2).domain)
    }

    test ("withRange of singleton ref-ref relation of element has correct domain") {
        assertSameCollection (Seq (num4)) (singleNumNum.withRange (num5).domain)
    }

    test ("withRange of singleton ref-ref relation of element has correct range") {
        assertSameCollection (Seq (num5)) (singleNumNum.withRange (num5).range)
    }

    test ("withRange of singleton ref-ref relation of non-element is empty") {
        assertResult (Seq ()) (singleNumNum.withRange (num4).domain)
    }

    test ("withRange of multiple element value-value relation of first element has correct domain") {
        assertResult (Seq (2)) (multiIntBool.withRange (false).domain)
    }

    test ("withRange of multiple element value-value relation of second element has correct domain") {
        assertResult (Seq (1)) (multiIntBool.withRange (true).domain)
    }

    test ("withRange of multiple element value-value relation of first element has correct range") {
        assertResult (Seq (false)) (multiIntBool.withRange (false).range)
    }

    test ("withRange of multiple element value-value relation of second element has correct range") {
        assertResult (Seq (true)) (multiIntBool.withRange (true).range)
    }

    test ("withRange of multiple element ref-value relation of first element has correct domain") {
        assertSameCollection (Seq (num2)) (multiNumInt.withRange (2).domain)
    }

    test ("withRange of multiple element ref-value relation of second element has correct domain") {
        assertSameCollection (Seq (num3)) (multiNumInt.withRange (3).domain)
    }

    test ("withRange of multiple element ref-value relation of first element has correct range") {
        assertResult (Seq (2)) (multiNumInt.withRange (2).range)
    }

    test ("withRange of multiple element ref-value relation of second element has correct range") {
        assertResult (Seq (3)) (multiNumInt.withRange (3).range)
    }

    test ("withRange of multiple element ref-value relation of non-element is empty") {
        assertResult (Seq ()) (multiNumInt.withRange (4).domain)
    }

    test ("withRange of multiple element value-ref relation of first element has correct domain") {
        assertResult (Seq (false)) (multiBoolNum.withRange (num3).domain)
    }

    test ("withRange of multiple element value-ref relation of second element has correct domain") {
        assertResult (Seq (false, true)) (multiBoolNum.withRange (num4).domain)
    }

    test ("withRange of multiple element value-ref relation of first element has correct range") {
        assertSameCollection (Seq (num3)) (multiBoolNum.withRange (num3).range)
    }

    test ("withRange of multiple element value-ref relation of second element has correct range") {
        assertSameCollection (Seq (num4)) (multiBoolNum.withRange (num4).range)
    }

    test ("withRange of multiple element ref-ref relation of element has correct domain") {
        assertSameCollection (Seq (num4)) (multiNumNum.withRange (num5).domain)
    }

    test ("withRange of multiple element ref-ref relation of element has correct range") {
        assertSameCollection (Seq (num5)) (multiNumNum.withRange (num5).range)
    }

    test ("withRange of multiple element ref-ref relation of non-element is empty") {
        assertSameCollection (Seq ()) (multiNumNum.withRange (num4).domain)
    }

}

/**
 * Helper routines for tests involving relations.
 */
trait RelationTestSupport {

    self : Tests =>

    import scala.language.higherKinds

    /**
     * Assert that a relation has a given image at `t`. By default, the expected
     * image is empty, so we are checking if the relation is not defined at `t`.
     */
    def assertImage[T,Repr[_,_]] (v : RelationLike[T,T,Repr], t : T, expected : Seq[T] = Seq ()) {
        assertSameCollection (expected) (v.image (t))
    }

    /**
     * Assert that a relation doesn't have a given image at `t`. By default, the
     * expected image is empty, so we are checking if the relation is defined at
     * `t`.
     */
    def assertNotImage[T,Repr[_,_]] (v : RelationLike[T,T,Repr], t : T, expected : Seq[T] = Seq ()) {
        assertNotSameCollection (expected) (v.image (t))
    }

}
