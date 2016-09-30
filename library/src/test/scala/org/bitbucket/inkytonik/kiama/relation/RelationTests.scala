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
package relation

import org.bitbucket.inkytonik.kiama.util.Tests

/**
 * Tests of binary relations.
 */
class RelationTests extends Tests {

    import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree.Num

    // Empty relations

    val emptyIntBool = new Relation[Int, Boolean](Vector())
    val emptyNumInt = new Relation[Num, Int](Vector())
    val emptyBoolNum = new Relation[Boolean, Num](Vector())
    val emptyNumNum = new Relation[Num, Num](Vector())

    // Singleton relations

    val num2 = Num(2)
    val num3 = Num(2) // deliberately == to num2, but not same
    val num4 = Num(4)
    val num5 = Num(5)

    val singleIntBool = new Relation[Int, Boolean](Vector((1, true)))
    val singleNumInt = new Relation[Num, Int](Vector((num2, 2)))
    val singleBoolNum = new Relation[Boolean, Num](Vector((false, num3)))
    val singleNumNum = new Relation[Num, Num](Vector((num4, num5)))

    // Multiple element relations

    val multiIntBool = new Relation[Int, Boolean](Vector((1, true), (2, false), (1, true)))
    val multiNumInt = new Relation[Num, Int](Vector((num2, 2), (num3, 3)))
    val multiBoolNum = new Relation[Boolean, Num](Vector((false, num3), (false, num4), (true, num4)))
    val multiNumNum = new Relation[Num, Num](Vector((num4, num5), (num4, num5)))

    // collect

    test("collect on an empty relation produces an empty relation") {
        emptyBoolNum.collect { case (b, n) => (!b, n) } shouldBe empty
    }

    test("collect same type on an singleton relation produces correct singleton relation") {
        singleBoolNum.collect { case (b, n) => (!b, n) }.graph shouldBe Vector((true, num3))
    }

    test("collect new type on an singleton relation produces correct singleton relation") {
        singleBoolNum.collect { case (b, n) => (n, !b) }.graph shouldBe Vector((num3, true))
    }

    test("collect same type on a multiple relation produces correct multiple relation (same Nums)") {
        multiNumInt.collect { case (n, i) => (n, i + 1) }.graph shouldBe Vector((num2, 3), (num3, 4))
    }

    test("collect same type on a multiple relation produces correct multiple relation (new Nums)") {
        multiNumInt.collect { case (Num(i), j) => (Num(i + 1), j + 1) }.graph shouldBe Vector((Num(3), 3), (Num(3), 4))
    }

    test("collect new type on a multiple relation produces correct multiple relation") {
        multiIntBool.collect { case (i, b) => (!b, "bob") }.graph shouldBe Vector((false, "bob"), (true, "bob"), (false, "bob"))
    }

    // compose

    test("compose two empty relations produces an empty relation ") {
        emptyBoolNum.compose(emptyIntBool) shouldBe empty
    }

    test("compose an empty relation on left of singleton relation produces an empty relation ") {
        emptyIntBool.compose(singleNumInt) shouldBe empty
    }

    test("compose an empty relation on right of singleton relation produces an empty relation ") {
        singleBoolNum.compose(emptyIntBool) shouldBe empty
    }

    test("compose a singleton relation with a mu;tiple relation produces the correct pairs") {
        multiIntBool.compose(singleNumInt).graph shouldBe Vector((num2, false))
    }

    test("compose of two multiple relations produces the correct pairs") {
        multiBoolNum.compose(multiIntBool).graph shouldBe Vector((1, num4), (2, num3), (2, num4), (1, num4))
    }

    // containsInDomain

    test("domain of singleton value-value relation contains its element") {
        singleIntBool.containsInDomain(1) shouldBe true
    }

    test("domain of singleton value-value relation doesn't contain a non-element") {
        singleIntBool.containsInDomain(2) shouldBe false
    }

    test("domain of singleton ref-value relation contains its element") {
        singleNumInt.containsInDomain(num2) shouldBe true
    }

    test("domain of singleton ref-value relation doesn't contain a non-element") {
        singleNumInt.containsInDomain(num3) shouldBe false
    }

    test("domain of singleton value-ref relation contains its element") {
        singleBoolNum.containsInDomain(false) shouldBe true
    }

    test("domain of singleton value-ref relation doesn't contain a non-element") {
        singleBoolNum.containsInDomain(true) shouldBe false
    }

    test("domain of singleton ref-ref relation contains its element") {
        singleNumNum.containsInDomain(num4) shouldBe true
    }

    test("domain of singleton ref-ref relation doesn't contain a non-element") {
        singleNumNum.containsInDomain(num5) shouldBe false
    }

    test("domain of multiple element value-value relation contains its first element") {
        multiIntBool.containsInDomain(1) shouldBe true
    }

    test("domain of multiple element value-value relation contains its second element") {
        multiIntBool.containsInDomain(2) shouldBe true
    }

    test("domain of multiple element value-value relation doesn't contain a non-element") {
        multiIntBool.containsInDomain(3) shouldBe false
    }

    test("domain of multiple element ref-value relation contains its first element") {
        multiNumInt.containsInDomain(num2) shouldBe true
    }

    test("domain of multiple element ref-value relation contains its second element") {
        multiNumInt.containsInDomain(num3) shouldBe true
    }

    test("domain of multiple element ref-value relation doesn't contain a non-element") {
        multiNumInt.containsInDomain(num4) shouldBe false
    }

    test("domain of multiple element value-ref relation contains its first element") {
        multiBoolNum.containsInDomain(false) shouldBe true
    }

    test("domain of multiple element value-ref relation contains its second element") {
        multiBoolNum.containsInDomain(true) shouldBe true
    }

    test("domain of multiple element ref-ref relation contains its element") {
        multiNumNum.containsInDomain(num4) shouldBe true
    }

    test("domain of multiple element ref-ref relation doesn't contain a non-element") {
        multiNumNum.containsInDomain(num5) shouldBe false
    }

    // containsInRange

    test("range of singleton value-value relation contains its element") {
        singleIntBool.containsInRange(true) shouldBe true
    }

    test("range of singleton value-value relation doesn't contain a non-element") {
        singleIntBool.containsInRange(false) shouldBe false
    }

    test("range of singleton ref-value relation contains its element") {
        singleNumInt.containsInRange(2) shouldBe true
    }

    test("range of singleton ref-value relation doesn't contain a non-element") {
        singleNumInt.containsInRange(3) shouldBe false
    }

    test("range of singleton value-ref relation contains its element") {
        singleBoolNum.containsInRange(num3) shouldBe true
    }

    test("range of singleton value-ref relation doesn't contain a non-element") {
        singleBoolNum.containsInRange(num2) shouldBe false
    }

    test("range of singleton ref-ref relation contains its element") {
        singleNumNum.containsInRange(num5) shouldBe true
    }

    test("range of singleton ref-ref relation doesn't contain a non-element") {
        singleNumNum.containsInRange(num4) shouldBe false
    }

    test("range of multiple element value-value relation contains its first element") {
        multiIntBool.containsInRange(true) shouldBe true
    }

    test("range of multiple element value-value relation contains its second element") {
        multiIntBool.containsInRange(false) shouldBe true
    }

    test("range of multiple element ref-value relation contains its first element") {
        multiNumInt.containsInRange(2) shouldBe true
    }

    test("range of multiple element ref-value relation contains its second element") {
        multiNumInt.containsInRange(3) shouldBe true
    }

    test("range of multiple element ref-value relation doesn't contain a non-element") {
        multiNumInt.containsInRange(4) shouldBe false
    }

    test("range of multiple element value-ref relation contains its first element") {
        multiBoolNum.containsInRange(num3) shouldBe true
    }

    test("range of multiple element value-ref relation contains its second element") {
        multiBoolNum.containsInRange(num4) shouldBe true
    }

    test("range of multiple element value-ref relation doesn't contain a non-element") {
        multiBoolNum.containsInRange(num2) shouldBe false
    }

    test("range of multiple element ref-ref relation contains its element") {
        multiNumNum.containsInRange(num5) shouldBe true
    }

    test("range of multiple element ref-ref relation doesn't contain a non-element") {
        multiNumNum.containsInRange(num4) shouldBe false
    }

    // domain

    test("domain of empty value-value relation is empty") {
        emptyIntBool.domain shouldBe empty
    }

    test("domain of empty ref-value relation is empty") {
        emptyNumInt.domain shouldBe empty
    }

    test("domain of empty value-ref relation is empty") {
        emptyBoolNum.domain shouldBe empty
    }

    test("domain of empty ref-ref relation is empty") {
        emptyNumNum.domain shouldBe empty
    }

    test("domain of singleton value-value relation is correct") {
        singleIntBool.domain shouldBe Vector(1)
    }

    test("domain of singleton ref-value relation is correct") {
        singleNumInt.domain should beSameCollectionAs(Vector(num2))
    }

    test("domain of singleton value-ref relation is correct") {
        singleBoolNum.domain shouldBe Vector(false)
    }

    test("domain of singleton ref-ref relation is correct") {
        singleNumNum.domain should beSameCollectionAs(Vector(num4))
    }

    test("domain of multiple element value-value relation is correct") {
        multiIntBool.domain shouldBe Vector(1, 2)
    }

    test("domain of multiple element ref-value relation is correct") {
        multiNumInt.domain should beSameCollectionAs(Vector(num2, num3))
    }

    test("domain of multiple element value-ref relation is correct") {
        multiBoolNum.domain shouldBe Vector(false, true)
    }

    test("domain of multiple element ref-ref relation is correct") {
        multiNumNum.domain should beSameCollectionAs(Vector(num4))
    }

    // image

    test("image of empty value-value relation is empty") {
        emptyIntBool.image(1) shouldBe empty
    }

    test("image of empty ref-value relation is empty") {
        emptyNumInt.image(num2) shouldBe empty
    }

    test("image of empty value-ref relation is empty") {
        emptyBoolNum.image(false) shouldBe empty
    }

    test("image of empty ref-ref relation is empty") {
        emptyNumNum.image(num3) shouldBe empty
    }

    test("image of singleton value-value relation is correct (present)") {
        singleIntBool.image(1) shouldBe Vector(true)
    }

    test("image of singleton value-value relation is empty (not present)") {
        singleIntBool.image(2) shouldBe empty
    }

    test("image of singleton ref-value relation is correct (present)") {
        singleNumInt.image(num2) shouldBe Vector(2)
    }

    test("image of singleton ref-value relation is empty (not present)") {
        singleNumInt.image(num3) shouldBe empty
    }

    test("image of singleton value-ref relation is correct (present)") {
        singleBoolNum.image(false) should beSameCollectionAs(Vector(num3))
    }

    test("image of singleton value-ref relation is empty (not present)") {
        singleBoolNum.image(true) shouldBe empty
    }

    test("image of singleton ref-ref relation is correct (present)") {
        singleNumNum.image(num4) should beSameCollectionAs(Vector(num5))
    }

    test("image of singleton ref-ref relation is empty (not present)") {
        singleNumNum.image(num5) shouldBe empty
    }

    test("image of multiple element value-value relation is correct (present 1)") {
        multiIntBool.image(1) shouldBe Vector(true, true)
    }

    test("image of multiple element value-value relation is correct (present 2)") {
        multiIntBool.image(2) shouldBe Vector(false)
    }

    test("image of multiple element value-value relation is empty (not present)") {
        multiIntBool.image(3) shouldBe empty
    }

    test("image of multiple element ref-value relation is correct (present)") {
        multiNumInt.image(num2) shouldBe Vector(2)
    }

    test("image of multiple element ref-value relation is empty (not present)") {
        multiNumInt.image(num4) shouldBe empty
    }

    test("image of multiple element value-ref relation is correct (present 1)") {
        multiBoolNum.image(false) should beSameCollectionAs(Vector(num3, num4))
    }

    test("image of multiple element value-ref relation is correct (present 2)") {
        multiBoolNum.image(true) should beSameCollectionAs(Vector(num4))
    }

    test("image of multiple element ref-ref relation is correct (present)") {
        multiNumNum.image(num4) should beSameCollectionAs(Vector(num5, num5))
    }

    test("image of multiple element ref-ref relation is empty (not present)") {
        multiNumNum.image(num2) shouldBe empty
    }

    // index

    test("index of empty value-value relation is empty") {
        emptyIntBool.index shouldBe empty
    }

    test("index of empty ref-value relation is empty") {
        emptyNumInt.index shouldBe empty
    }

    test("index of empty value-ref relation is empty") {
        emptyBoolNum.index shouldBe empty
    }

    test("index of empty ref-ref relation is empty") {
        emptyNumNum.index shouldBe empty
    }

    test("index of singleton value-value relation is correct") {
        singleIntBool.index.graph shouldBe Vector((true, 0))
    }

    test("index of singleton ref-value relation is correct") {
        singleNumInt.index.graph shouldBe Vector((2, 0))
    }

    test("index of singleton value-ref relation is correct") {
        singleBoolNum.index.graph should beSameCollectionAs(Vector((num3, 0)))
    }

    test("index of singleton ref-ref relation is correct (present)") {
        singleNumNum.index.graph should beSameCollectionAs(Vector((num5, 0)))
    }

    test("index of multiple element value-value relation is correct") {
        multiIntBool.index.graph shouldBe Vector((true, 0), (false, 1), (true, 2))
    }

    test("index of multiple element ref-value relation is correct") {
        multiNumInt.index.graph shouldBe Vector((2, 0), (3, 1))
    }

    test("index of multiple element value-ref relation is correct") {
        multiBoolNum.index.graph should beSameCollectionAs(Vector((num3, 0), (num4, 1), (num4, 2)))
    }

    test("index of multiple element ref-ref relation is correct") {
        multiNumNum.index.graph should beSameCollectionAs(Vector((num5, 0), (num5, 1)))
    }

    // inverse

    test("inverting an empty relation yields an empty relation") {
        emptyIntBool.inverse shouldBe empty
    }

    test("inverting a singleton relation yields the correct singleton relation") {
        singleIntBool.inverse.graph shouldBe Vector((true, 1))
    }

    test("inverting a multiple relation yields the correct multiple relation") {
        multiBoolNum.inverse.graph shouldBe Vector((num3, false), (num4, false), (num4, true))
    }

    // preImage

    test("preImage of empty value-value relation is empty") {
        emptyIntBool.preImage(false) shouldBe empty
    }

    test("preImage of empty ref-value relation is empty") {
        emptyNumInt.preImage(2) shouldBe empty
    }

    test("preImage of empty value-ref relation is empty") {
        emptyBoolNum.preImage(num2) shouldBe empty
    }

    test("preImage of empty ref-ref relation is empty") {
        emptyNumNum.preImage(num3) shouldBe empty
    }

    test("preImage of singleton value-value relation is correct (present)") {
        singleIntBool.preImage(true) shouldBe Vector(1)
    }

    test("preImage of singleton value-value relation is empty (not present)") {
        singleIntBool.preImage(false) shouldBe empty
    }

    test("preImage of singleton ref-value relation is correct (present)") {
        singleNumInt.preImage(2) should beSameCollectionAs(Vector(num2))
    }

    test("preImage of singleton ref-value relation is empty (not present)") {
        singleNumInt.preImage(3) shouldBe empty
    }

    test("preImage of singleton value-ref relation is correct (present)") {
        singleBoolNum.preImage(num3) shouldBe Vector(false)
    }

    test("preImage of singleton value-ref relation is empty (not present)") {
        singleBoolNum.preImage(num2) shouldBe empty
    }

    test("preImage of singleton ref-ref relation is correct (present)") {
        singleNumNum.preImage(num5) should beSameCollectionAs(Vector(num4))
    }

    test("preImage of singleton ref-ref relation is empty (not present)") {
        singleNumNum.preImage(num4) shouldBe empty
    }

    test("preImage of multiple element value-value relation is correct (present 1)") {
        multiIntBool.preImage(true) shouldBe Vector(1, 1)
    }

    test("preImage of multiple element value-value relation is correct (present 2)") {
        multiIntBool.preImage(false) shouldBe Vector(2)
    }

    test("preImage of multiple element ref-value relation is correct (present)") {
        multiNumInt.preImage(2) should beSameCollectionAs(Vector(num2))
    }

    test("preImage of multiple element ref-value relation is empty (not present)") {
        multiNumInt.preImage(4) shouldBe empty
    }

    test("preImage of multiple element value-ref relation is correct (present 1)") {
        multiBoolNum.preImage(num3) shouldBe Vector(false)
    }

    test("preImage of multiple element value-ref relation is correct (present 2)") {
        multiBoolNum.preImage(num4) shouldBe Vector(false, true)
    }

    test("preImage of multiple element value-ref relation is empty (not present)") {
        multiBoolNum.preImage(num2) shouldBe empty
    }

    test("preImage of multiple element ref-ref relation is correct (present)") {
        multiNumNum.preImage(num5) should beSameCollectionAs(Vector(num4, num4))
    }

    test("preImage of multiple element ref-ref relation is empty (not present)") {
        multiNumNum.preImage(num2) shouldBe empty
    }

    // preIndex

    test("preIndex of empty value-value relation is empty") {
        emptyIntBool.preIndex shouldBe empty
    }

    test("preIndex of empty ref-value relation is empty") {
        emptyNumInt.preIndex shouldBe empty
    }

    test("preIndex of empty value-ref relation is empty") {
        emptyBoolNum.preIndex shouldBe empty
    }

    test("preIndex of empty ref-ref relation is empty") {
        emptyNumNum.preIndex shouldBe empty
    }

    test("preIndex of singleton value-value relation is correct") {
        singleIntBool.preIndex.graph shouldBe Vector((1, 0))
    }

    test("preIndex of singleton ref-value relation is correct") {
        singleNumInt.preIndex.graph should beSameCollectionAs(Vector((num2, 0)))
    }

    test("preIndex of singleton value-ref relation is correct") {
        singleBoolNum.preIndex.graph shouldBe Vector((false, 0))
    }

    test("preIndex of singleton ref-ref relation is correct (present)") {
        singleNumNum.preIndex.graph should beSameCollectionAs(Vector((num4, 0)))
    }

    test("preIndex of multiple element value-value relation is correct") {
        multiIntBool.preIndex.graph shouldBe Vector((1, 0), (2, 1), (1, 2))
    }

    test("preIndex of multiple element ref-value relation is correct") {
        multiNumInt.preIndex.graph should beSameCollectionAs(Vector((num2, 0), (num3, 1)))
    }

    test("preIndex of multiple element value-ref relation is correct") {
        multiBoolNum.preIndex.graph shouldBe Vector((false, 0), (false, 1), (true, 2))
    }

    test("preIndex of multiple element ref-ref relation is correct") {
        multiNumNum.preIndex.graph should beSameCollectionAs(Vector((num4, 0), (num4, 1)))
    }

    // projDomain

    test("projDomain of empty value-value relation is empty") {
        emptyIntBool.projDomain shouldBe empty
    }

    test("projDomain of empty ref-value relation is empty") {
        emptyNumInt.projDomain shouldBe empty
    }

    test("projDomain of empty value-ref relation is empty") {
        emptyBoolNum.projDomain shouldBe empty
    }

    test("projDomain of empty ref-ref relation is empty") {
        emptyNumNum.projDomain shouldBe empty
    }

    test("projDomain of singleton value-value relation is correct") {
        singleIntBool.projDomain.graph shouldBe Vector((1, Vector(true)))
    }

    test("projDomain of singleton ref-value relation is correct") {
        singleNumInt.projDomain.graph should beSameCollectionAs(Vector((num2, Vector(2))))
    }

    test("projDomain of singleton value-ref relation is correct") {
        singleBoolNum.projDomain.graph should beSameCollectionAs(Vector((false, Vector(num3))))
    }

    test("projDomain of singleton ref-ref relation is correct") {
        singleNumNum.projDomain.graph should beSameCollectionAs(Vector((num4, Vector(num5))))
    }

    test("projDomain of multiple element value-value relation is correct") {
        multiIntBool.projDomain.graph shouldBe Vector((1, Vector(true, true)), (2, Vector(false)))
    }

    test("projDomain of multiple element ref-value relation is correct") {
        multiNumInt.projDomain.graph should beSameCollectionAs(Vector((num2, Vector(2)), (num3, Vector(3))))
    }

    test("projDomain of multiple element value-ref relation is correct") {
        multiBoolNum.projDomain.graph should beSameCollectionAs(Vector((false, Vector(num3, num4)), (true, Vector(num4))))
    }

    test("projDomain of multiple element ref-ref relation is correct") {
        multiNumNum.projDomain.graph should beSameCollectionAs(Vector((num4, Vector(num5, num5))))
    }

    // projRange

    test("projRange of empty value-value relation is empty") {
        emptyIntBool.projRange shouldBe empty
    }

    test("projRange of empty ref-value relation is empty") {
        emptyNumInt.projRange shouldBe empty
    }

    test("projRange of empty value-ref relation is empty") {
        emptyBoolNum.projRange shouldBe empty
    }

    test("projRange of empty ref-ref relation is empty") {
        emptyNumNum.projRange shouldBe empty
    }

    test("projRange of singleton value-value relation is correct") {
        singleIntBool.projRange.graph shouldBe Vector((true, Vector(1)))
    }

    test("projRange of singleton ref-value relation is correct") {
        singleNumInt.projRange.graph should beSameCollectionAs(Vector((2, Vector(num2))))
    }

    test("projRange of singleton value-ref relation is correct") {
        singleBoolNum.projRange.graph should beSameCollectionAs(Vector((num3, Vector(false))))
    }

    test("projRange of singleton ref-ref relation is correct") {
        singleNumNum.projRange.graph should beSameCollectionAs(Vector((num5, Vector(num4))))
    }

    test("projRange of multiple element value-value relation is correct") {
        multiIntBool.projRange.graph shouldBe Vector((true, Vector(1, 1)), (false, Vector(2)))
    }

    test("projRange of multiple element ref-value relation is correct") {
        multiNumInt.projRange.graph should beSameCollectionAs(Vector((2, Vector(num2)), (3, Vector(num3))))
    }

    test("projRange of multiple element value-ref relation is correct") {
        multiBoolNum.projRange.graph should beSameCollectionAs(Vector((num3, Vector(false)), (num4, Vector(false, true))))
    }

    test("projRange of multiple element ref-ref relation is correct") {
        multiNumNum.projRange.graph should beSameCollectionAs(Vector((num5, Vector(num4, num4))))
    }

    // range

    test("range of empty value-value relation is empty") {
        emptyIntBool.range shouldBe empty
    }

    test("range of empty ref-value relation is empty") {
        emptyNumInt.range shouldBe empty
    }

    test("range of empty value-ref relation is empty") {
        emptyBoolNum.range shouldBe empty
    }

    test("range of empty ref-ref relation is empty") {
        emptyNumNum.range shouldBe empty
    }

    test("range of singleton value-value relation is correct") {
        singleIntBool.range shouldBe Vector(true)
    }

    test("range of singleton ref-value relation is correct") {
        singleNumInt.range shouldBe Vector(2)
    }

    test("range of singleton value-ref relation is correct") {
        singleBoolNum.range should beSameCollectionAs(Vector(num3))
    }

    test("range of singleton ref-ref relation is correct") {
        singleNumNum.range should beSameCollectionAs(Vector(num5))
    }

    test("range of multiple element value-value relation is correct") {
        multiIntBool.range shouldBe Vector(true, false)
    }

    test("range of multiple element ref-value relation is correct") {
        multiNumInt.range shouldBe Vector(2, 3)
    }

    test("range of multiple element value-ref relation is correct") {
        multiBoolNum.range should beSameCollectionAs(Vector(num3, num4))
    }

    test("range of multiple element ref-ref relation is correct") {
        multiNumNum.range should beSameCollectionAs(Vector(num5))
    }

    // unapplySeq (direct)

    test("unapplySeq of empty value-value relation fails") {
        emptyIntBool.unapplySeq(1) shouldBe Some(Vector())
    }

    test("unapplySeq of empty ref-value relation fails") {
        emptyNumInt.unapplySeq(num2) shouldBe Some(Vector())
    }

    test("unapplySeq of empty value-ref relation fails") {
        emptyBoolNum.unapplySeq(false) shouldBe Some(Vector())
    }

    test("unapplySeq of empty ref-ref relation fails") {
        emptyNumNum.unapplySeq(num3) shouldBe Some(Vector())
    }

    test("unapplySeq of singleton value-value relation is correct (present)") {
        singleIntBool.unapplySeq(1) shouldBe Some(Vector(true))
    }

    test("unapplySeq pair of singleton value-value relation is correct (present)") {
        singleIntBool.pair.unapplySeq(1) shouldBe Some((1, Vector(true)))
    }

    test("unapplySeq of singleton value-value relation fails (not present)") {
        singleIntBool.unapplySeq(2) shouldBe Some(Vector())
    }

    test("unapplySeq of singleton ref-value relation is correct (present)") {
        singleNumInt.unapplySeq(num2) shouldBe Some(Vector(2))
    }

    // This test makes sure that we are comparing nodes by identity since
    // num2 and num3 are equal by value.

    test("unapplySeq pair of singleton ref-value relation doesn't produce equal but not eq value (present)") {
        singleNumInt.pair.unapplySeq(num2) should not(beSameCollectionAs(Some((num3, Vector(2)))))
    }

    test("unapplySeq of singleton ref-value relation fails (not present)") {
        singleNumInt.unapplySeq(num3) shouldBe Some(Vector())
    }

    test("unapplySeq of singleton value-ref relation is correct (present)") {
        singleBoolNum.unapplySeq(false) should beSameCollectionAs(Some(Vector(num3)))
    }

    test("unapplySeq pair of singleton value-ref relation is correct (present)") {
        singleBoolNum.pair.unapplySeq(false) should beSameCollectionAs(Some((false, Vector(num3))))
    }

    test("unapplySeq of singleton value-ref relation fails (not present)") {
        singleBoolNum.unapplySeq(true) shouldBe Some(Vector())
    }

    test("unapplySeq of singleton ref-ref relation is correct (present)") {
        singleNumNum.unapplySeq(num4) should beSameCollectionAs(Some(Vector(num5)))
    }

    test("unapplySeq pair of singleton ref-ref relation is correct (present)") {
        singleNumNum.pair.unapplySeq(num4) should beSameCollectionAs(Some((num4, Vector(num5))))
    }

    test("unapplySeq of singleton ref-ref relation fails (not present)") {
        singleNumNum.unapplySeq(num5) shouldBe Some(Vector())
    }

    test("unapplySeq of multiple element value-value relation fails (multiple)") {
        multiIntBool.unapplySeq(1) shouldBe Some(Vector(true, true))
    }

    test("unapplySeq of multiple element value-value relation is correct (present)") {
        multiIntBool.unapplySeq(2) shouldBe Some(Vector(false))
    }

    test("unapplySeq pair of multiple element value-value relation is correct (present)") {
        multiIntBool.pair.unapplySeq(2) shouldBe Some((2, Vector(false)))
    }

    test("unapplySeq of multiple element value-value relation fails (not present)") {
        multiIntBool.unapplySeq(3) shouldBe Some(Vector())
    }

    test("unapplySeq of multiple element ref-value relation is correct (present)") {
        multiNumInt.unapplySeq(num2) shouldBe Some(Vector(2))
    }

    test("unapplySeq pair of multiple element ref-value relation is correct (present)") {
        multiNumInt.pair.unapplySeq(num2) should beSameCollectionAs(Some((num2, Vector(2))))
    }

    test("unapplySeq of multiple element ref-value relation is correct (not present)") {
        multiNumInt.unapplySeq(num4) shouldBe Some(Vector())
    }

    test("unapplySeq of multiple element value-ref relation fails (multiple)") {
        multiBoolNum.unapplySeq(false) should beSameCollectionAs(Some(Vector(num3, num4)))
    }

    test("unapplySeq of multiple element value-ref relation is correct (present)") {
        multiBoolNum.unapplySeq(true) should beSameCollectionAs(Some(Vector(num4)))
    }

    test("unapplySeq pair of multiple element value-ref relation is correct (present)") {
        multiBoolNum.pair.unapplySeq(true) should beSameCollectionAs(Some((true, Vector(num4))))
    }

    test("unapplySeq of multiple element ref-ref relation is correct (multiple)") {
        multiNumNum.unapplySeq(num4) should beSameCollectionAs(Some(Vector(num5, num5)))
    }

    test("unapplySeq of multiple element ref-ref relation is correct (not present)") {
        multiNumNum.unapplySeq(num2) shouldBe Some(Vector())
    }

    // unapplySeq (via match)

    test("match of an empty relation gives nothing") {
        (1 match {
            case emptyIntBool() => true
            case _              => false
        }) shouldBe true
    }

    test("match of a singleton relation suceeds with exactly one value (present)") {
        (num2 match {
            case singleNumInt(v) => v
            case _               => 0
        }) shouldBe 2
    }

    test("pair match of a singleton relation suceeds with exactly one value (present)") {
        (num2 match {
            case singleNumInt.pair(n, v) => (n, v)
            case _                       => (Num(0), 0)
        }) should beSameCollectionAs((num2, 2))
    }

    test("match of a singleton relation gives nothing (not present)") {
        (num3 match {
            case singleNumInt() => true
            case _              => false
        }) shouldBe true
    }

    test("match of a multiple relation gives multiple values (present)") {
        (1 match {
            case multiIntBool(v1, v2) => (v1, v2)
            case _                    => (false, false)
        }) shouldBe ((true, true))
    }

    test("pair match of a multiple relation gives multiple values (present)") {
        (1 match {
            case multiIntBool.pair(i, v1, v2) => (i, v1, v2)
            case _                            => (0, false, false)
        }) shouldBe ((1, true, true))
    }

    test("match of a multiple relation fails (not present)") {
        (3 match {
            case multiIntBool() => true
            case _              => false
        }) shouldBe true
    }

    // union

    test("an empty relation union an empty relation is empty (value-value)") {
        val r = new Relation[Int, Boolean](Vector())
        emptyIntBool.union(r) shouldBe empty
    }

    test("an empty relation union an empty relation is empty (ref-value)") {
        val r = new Relation[Num, Int](Vector())
        emptyNumInt.union(r) shouldBe empty
    }

    test("an empty relation union an empty relation is empty (value-ref)") {
        val r = new Relation[Boolean, Num](Vector())
        emptyBoolNum.union(r) shouldBe empty
    }

    test("an empty relation union an empty relation is empty (ref-ref)") {
        val r = new Relation[Num, Num](Vector())
        emptyNumNum.union(r) shouldBe empty
    }

    test("an empty relation union a non-empty relation has correct graph (value-value)") {
        emptyIntBool.union(singleIntBool).graph shouldBe Vector((1, true))
    }

    test("a non-empty relation union an empty relation has correct graph (value-value)") {
        singleIntBool.union(emptyIntBool).graph shouldBe Vector((1, true))
    }

    test("an empty relation union a non-empty relation has correct graph (ref-value)") {
        emptyNumInt.union(singleNumInt).graph should beSameCollectionAs(Vector((num2, 2)))
    }

    test("a non-empty relation union an empty relation has correct graph (ref-value)") {
        singleNumInt.union(emptyNumInt).graph should beSameCollectionAs(Vector((num2, 2)))
    }

    test("an empty relation union a non-empty relation has correct graph (value-ref)") {
        emptyBoolNum.union(singleBoolNum).graph should beSameCollectionAs(Vector((false, num3)))
    }

    test("a non-empty relation union an empty relation has correct graph (value-ref)") {
        singleBoolNum.union(emptyBoolNum).graph should beSameCollectionAs(Vector((false, num3)))
    }

    test("an empty relation union a non-empty relation has correct graph (ref-ref)") {
        emptyNumNum.union(singleNumNum).graph should beSameCollectionAs(Vector((num4, num5)))
    }

    test("a non-empty relation union an empty relation has correct graph (ref-ref)") {
        singleNumNum.union(emptyNumNum).graph should beSameCollectionAs(Vector((num4, num5)))
    }

    test("union of non-empty relations has correct graph (value-value)") {
        val r = new Relation[Int, Boolean](Vector((42, false), (99, true)))
        multiIntBool.union(r).graph shouldBe Vector((1, true), (2, false), (1, true), (42, false), (99, true))
    }

    test("union of non-empty relations has correct graph (ref-value)") {
        val r = new Relation[Num, Int](Vector((num4, 42)))
        multiNumInt.union(r).graph should beSameCollectionAs(Vector((num2, 2), (num3, 3), (num4, 42)))
    }

    test("union of non-empty relations has correct graph (value-ref)") {
        val r = new Relation[Boolean, Num](Vector((false, num3), (true, num2)))
        multiBoolNum.union(r).graph should beSameCollectionAs(Vector((false, num3), (false, num4), (true, num4), (false, num3), (true, num2)))
    }

    test("union of non-empty relations has correct graph (ref-ref)") {
        val r = new Relation[Num, Num](Vector((num2, num3), (num2, num3)))
        multiNumNum.union(r).graph should beSameCollectionAs(Vector((num4, num5), (num4, num5), (num2, num3), (num2, num3)))
    }

    // withDomain

    test("withDomain of empty value-value relation is an empty relation") {
        emptyIntBool.withDomain(1) shouldBe empty
    }

    test("withDomain of empty ref-value relation is an empty relation") {
        emptyNumInt.withDomain(num2) shouldBe empty
    }

    test("withDomain of empty value-ref relation is an empty relation") {
        emptyBoolNum.withDomain(true) shouldBe empty
    }

    test("withDomain of empty ref-ref relation is an empty relation") {
        emptyNumNum.withDomain(num4) shouldBe empty
    }

    test("withDomain of singleton value-value relation of element has correct domain") {
        singleIntBool.withDomain(1).domain shouldBe Vector(1)
    }

    test("withDomain of singleton value-value relation of element has correct range") {
        singleIntBool.withDomain(1).range shouldBe Vector(true)
    }

    test("withDomain of singleton value-value relation of non-element is empty") {
        singleIntBool.withDomain(2).domain shouldBe Vector()
    }

    test("withDomain of singleton ref-value relation of element has correct domain") {
        singleNumInt.withDomain(num2).domain should beSameCollectionAs(Vector(num2))
    }

    test("withDomain of singleton ref-value relation of element has correct range") {
        singleNumInt.withDomain(num2).range shouldBe Vector(2)
    }

    test("withDomain of singleton ref-value relation of non-element is empty") {
        singleNumInt.withDomain(num3).domain shouldBe Vector()
    }

    test("withDomain of singleton value-ref relation of element has correct domain") {
        singleBoolNum.withDomain(false).domain shouldBe Vector(false)
    }

    test("withDomain of singleton value-ref relation of element has correct range") {
        singleBoolNum.withDomain(false).range should beSameCollectionAs(Vector(num3))
    }

    test("withDomain of singleton value-ref relation of non-element is empty") {
        singleBoolNum.withDomain(true).domain shouldBe Vector()
    }

    test("withDomain of singleton ref-ref relation of element has correct domain") {
        singleNumNum.withDomain(num4).domain should beSameCollectionAs(Vector(num4))
    }

    test("withDomain of singleton ref-ref relation of element has correct range") {
        singleNumNum.withDomain(num4).range should beSameCollectionAs(Vector(num5))
    }

    test("withDomain of singleton ref-ref relation of non-element is empty") {
        singleNumNum.withDomain(num5).domain shouldBe Vector()
    }

    test("withDomain of multiple element value-value relation of first element has correct domain") {
        multiIntBool.withDomain(1).domain shouldBe Vector(1)
    }

    test("withDomain of multiple element value-value relation of second element has correct domain") {
        multiIntBool.withDomain(2).domain shouldBe Vector(2)
    }

    test("withDomain of multiple element value-value relation of first element has correct range") {
        multiIntBool.withDomain(1).range shouldBe Vector(true)
    }

    test("withDomain of multiple element value-value relation of second element has correct range") {
        multiIntBool.withDomain(2).range shouldBe Vector(false)
    }

    test("withDomain of multiple element value-value relation of non-element is empty") {
        multiIntBool.withDomain(3).domain shouldBe Vector()
    }

    test("withDomain of multiple element ref-value relation of first element has correct domain") {
        multiNumInt.withDomain(num2).domain should beSameCollectionAs(Vector(num2))
    }

    test("withDomain of multiple element ref-value relation of second element has correct domain") {
        multiNumInt.withDomain(num3).domain should beSameCollectionAs(Vector(num3))
    }

    test("withDomain of multiple element ref-value relation of first element has correct range") {
        multiNumInt.withDomain(num2).range shouldBe Vector(2)
    }

    test("withDomain of multiple element ref-value relation of second element has correct range") {
        multiNumInt.withDomain(num3).range shouldBe Vector(3)
    }

    test("withDomain of multiple element ref-value relation of non-element is empty") {
        multiNumInt.withDomain(num4).domain shouldBe Vector()
    }

    test("withDomain of multiple element value-ref relation of first element has correct domain") {
        multiBoolNum.withDomain(false).domain shouldBe Vector(false)
    }

    test("withDomain of multiple element value-ref relation of second element has correct domain") {
        multiBoolNum.withDomain(true).domain shouldBe Vector(true)
    }

    test("withDomain of multiple element value-ref relation of first element has correct range") {
        multiBoolNum.withDomain(false).range should beSameCollectionAs(Vector(num3, num4))
    }

    test("withDomain of multiple element value-ref relation of second element has correct range") {
        multiBoolNum.withDomain(true).range should beSameCollectionAs(Vector(num4))
    }

    test("withDomain of multiple element ref-ref relation of element has correct domain") {
        multiNumNum.withDomain(num4).domain should beSameCollectionAs(Vector(num4))
    }

    test("withDomain of multiple element ref-ref relation of element has correct range") {
        multiNumNum.withDomain(num4).range should beSameCollectionAs(Vector(num5))
    }

    test("withDomain of multiple element ref-ref relation of non-element is empty") {
        multiNumNum.withDomain(num5).domain should beSameCollectionAs(Vector())
    }

    // withRange

    test("withRange of empty value-value relation is an empty relation") {
        emptyIntBool.withRange(true) shouldBe empty
    }

    test("withRange of empty ref-value relation is an empty relation") {
        emptyNumInt.withRange(2) shouldBe empty
    }

    test("withRange of empty value-ref relation is an empty relation") {
        emptyBoolNum.withRange(num2) shouldBe empty
    }

    test("withRange of empty ref-ref relation is an empty relation") {
        emptyNumNum.withRange(num4) shouldBe empty
    }

    test("withRange of singleton value-value relation of element has correct domain") {
        singleIntBool.withRange(true).domain shouldBe Vector(1)
    }

    test("withRange of singleton value-value relation of element has correct range") {
        singleIntBool.withRange(true).range shouldBe Vector(true)
    }

    test("withRange of singleton value-value relation of non-element is empty") {
        singleIntBool.withRange(false).domain shouldBe Vector()
    }

    test("withRange of singleton ref-value relation of element has correct domain") {
        singleNumInt.withRange(2).domain should beSameCollectionAs(Vector(num2))
    }

    test("withRange of singleton ref-value relation of element has correct range") {
        singleNumInt.withRange(2).range shouldBe Vector(2)
    }

    test("withRange of singleton ref-value relation of non-element is empty") {
        singleNumInt.withRange(3).domain shouldBe Vector()
    }

    test("withRange of singleton value-ref relation of element has correct domain") {
        singleBoolNum.withRange(num3).domain shouldBe Vector(false)
    }

    test("withRange of singleton value-ref relation of element has correct range") {
        singleBoolNum.withRange(num3).range should beSameCollectionAs(Vector(num3))
    }

    test("withRange of singleton value-ref relation of non-element is empty") {
        singleBoolNum.withRange(num2).domain shouldBe Vector()
    }

    test("withRange of singleton ref-ref relation of element has correct domain") {
        singleNumNum.withRange(num5).domain should beSameCollectionAs(Vector(num4))
    }

    test("withRange of singleton ref-ref relation of element has correct range") {
        singleNumNum.withRange(num5).range should beSameCollectionAs(Vector(num5))
    }

    test("withRange of singleton ref-ref relation of non-element is empty") {
        singleNumNum.withRange(num4).domain shouldBe Vector()
    }

    test("withRange of multiple element value-value relation of first element has correct domain") {
        multiIntBool.withRange(false).domain shouldBe Vector(2)
    }

    test("withRange of multiple element value-value relation of second element has correct domain") {
        multiIntBool.withRange(true).domain shouldBe Vector(1)
    }

    test("withRange of multiple element value-value relation of first element has correct range") {
        multiIntBool.withRange(false).range shouldBe Vector(false)
    }

    test("withRange of multiple element value-value relation of second element has correct range") {
        multiIntBool.withRange(true).range shouldBe Vector(true)
    }

    test("withRange of multiple element ref-value relation of first element has correct domain") {
        multiNumInt.withRange(2).domain should beSameCollectionAs(Vector(num2))
    }

    test("withRange of multiple element ref-value relation of second element has correct domain") {
        multiNumInt.withRange(3).domain should beSameCollectionAs(Vector(num3))
    }

    test("withRange of multiple element ref-value relation of first element has correct range") {
        multiNumInt.withRange(2).range shouldBe Vector(2)
    }

    test("withRange of multiple element ref-value relation of second element has correct range") {
        multiNumInt.withRange(3).range shouldBe Vector(3)
    }

    test("withRange of multiple element ref-value relation of non-element is empty") {
        multiNumInt.withRange(4).domain shouldBe Vector()
    }

    test("withRange of multiple element value-ref relation of first element has correct domain") {
        multiBoolNum.withRange(num3).domain shouldBe Vector(false)
    }

    test("withRange of multiple element value-ref relation of second element has correct domain") {
        multiBoolNum.withRange(num4).domain shouldBe Vector(false, true)
    }

    test("withRange of multiple element value-ref relation of first element has correct range") {
        multiBoolNum.withRange(num3).range should beSameCollectionAs(Vector(num3))
    }

    test("withRange of multiple element value-ref relation of second element has correct range") {
        multiBoolNum.withRange(num4).range should beSameCollectionAs(Vector(num4))
    }

    test("withRange of multiple element ref-ref relation of element has correct domain") {
        multiNumNum.withRange(num5).domain should beSameCollectionAs(Vector(num4))
    }

    test("withRange of multiple element ref-ref relation of element has correct range") {
        multiNumNum.withRange(num5).range should beSameCollectionAs(Vector(num5))
    }

    test("withRange of multiple element ref-ref relation of non-element is empty") {
        multiNumNum.withRange(num4).domain shouldBe Vector()
    }

}
