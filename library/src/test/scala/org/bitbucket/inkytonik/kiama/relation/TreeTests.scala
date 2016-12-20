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
 * Tests of tree relations.
 */
class TreeTests extends Tests {

    import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree._
    import org.bitbucket.inkytonik.kiama.relation.Tree.isLeaf
    import scala.collection.immutable.Set

    // Test tree

    val n1 = Num(1) // these are deliberately ==, but not same
    val n2 = Num(1)
    val n3 = Num(1)
    val n4 = Num(1)

    val v1 = Var("a")
    val v2 = Var("b")
    val v3 = Var("c")

    val e1 = Add(n1, n2)
    val e2 = Mul(e1, v1)
    val e3 = Neg(n3)
    val e4 = Var("d")

    val s1 = Asgn(v2, e2)
    val s2 = Null()
    val s3 = Asgn(v3, e3)
    val s4 = While(e4, s3)
    val s5 = Null()

    val nulls = Vector.fill(24)(Null())

    case class Program(u : Stmt, as : List[Stmt], v : Stmt, optb1 : Option[Stmt],
        optb2 : Option[Stmt], w : Stmt, eitherc1 : Either[Stmt, Stmt],
        eitherc2 : Either[Stmt, Stmt], x : Stmt,
        t1 : Tuple1[Stmt], t2 : (Stmt, Stmt), t3 : (Stmt, Stmt, Stmt),
        t4 : (Stmt, Stmt, Stmt, Stmt),
        y : Stmt, listopts : List[Option[Stmt]], z : Stmt,
        vec : Vector[Stmt], map : Map[Int, Stmt],
        cross : Bridge[Num]) extends ImperativeNode

    val p = Program(s1, List(s2, s4), s5, Some(nulls(0)), None, nulls(1),
        Left(nulls(2)), Right(nulls(3)), nulls(4),
        Tuple1(nulls(5)), (nulls(6), nulls(7)),
        (nulls(8), nulls(9), nulls(10)),
        (nulls(11), nulls(12), nulls(13), nulls(14)),
        nulls(15), List(Some(nulls(16)), Some(nulls(17))), nulls(18),
        Vector(nulls(19), nulls(20), nulls(21)),
        Map(1 -> nulls(22), 2 -> nulls(23)),
        Bridge(n4))

    val pchildren = Vector(s1, s2, s4, s5) ++ nulls

    val ptree = new ImperativeTree(p)
    import ptree._

    // A value that is not a node of the `p` tree
    val nonNode = Num(1)

    // child

    test("child of leaf is empty (n1)") {
        child.image(n1) shouldBe empty
    }

    test("child of leaf is empty (n2)") {
        child.image(n2) shouldBe empty
    }

    test("child of leaf is empty (n3)") {
        child.image(n3) shouldBe empty
    }

    test("child of leaf is empty (v1)") {
        child.image(v1) shouldBe empty
    }

    test("child of leaf is empty (v2)") {
        child.image(v2) shouldBe empty
    }

    test("child of leaf is empty (v3)") {
        child.image(v3) shouldBe empty
    }

    test("child of leaf is empty (e4)") {
        child.image(e4) shouldBe empty
    }

    test("child of leaf is empty (s2)") {
        child.image(s2) shouldBe empty
    }

    test("child of interior node is its children (e1)") {
        child.image(e1) should beSameCollectionAs(Vector(n1, n2))
    }

    test("child of interior node is its children (e2)") {
        child.image(e2) should beSameCollectionAs(Vector(e1, v1))
    }

    test("child of interior node is its children (e3)") {
        child.image(e3) should beSameCollectionAs(Vector(n3))
    }

    test("child of interior node is its children (s1)") {
        child.image(s1) should beSameCollectionAs(Vector(v2, e2))
    }

    test("child of interior node is its children (v3)") {
        child.image(s3) should beSameCollectionAs(Vector(v3, e3))
    }

    test("child of interior node is its children (s4)") {
        child.image(s4) should beSameCollectionAs(Vector(e4, s3))
    }

    test("child of Program ignores its non ImperativeTree fields") {
        child.image(p) should beSameCollectionAs(pchildren)
    }

    test("child of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            child(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // firstChild

    test("firstChild of a leaf is not defined (n1)") {
        firstChild.unapplySeq(n1) shouldBe Some(Vector())
    }

    test("firstChild of a leaf is not defined (n2)") {
        firstChild.unapplySeq(n2) shouldBe Some(Vector())
    }

    test("firstChild of a leaf is not defined (n3)") {
        firstChild.unapplySeq(n3) shouldBe Some(Vector())
    }

    test("firstChild of a leaf is not defined (v1)") {
        firstChild.unapplySeq(v1) shouldBe Some(Vector())
    }

    test("firstChild of a leaf is not defined (v2)") {
        firstChild.unapplySeq(v2) shouldBe Some(Vector())
    }

    test("firstChild of a leaf is not defined (v3)") {
        firstChild.unapplySeq(v3) shouldBe Some(Vector())
    }

    test("firstChild of a leaf is not defined (e4)") {
        firstChild.unapplySeq(e4) shouldBe Some(Vector())
    }

    test("firstChild of a leaf is not defined (s2)") {
        firstChild.unapplySeq(s2) shouldBe Some(Vector())
    }

    test("firstChild of a node with children is correct (e1)") {
        firstChild.unapplySeq(e1) should beSameCollectionAs(Some(Vector(n1)))
    }

    test("firstChild of a node with children is correct (e2)") {
        firstChild.unapplySeq(e2) should beSameCollectionAs(Some(Vector(e1)))
    }

    test("firstChild of a node with children is correct (e3)") {
        firstChild.unapplySeq(e3) should beSameCollectionAs(Some(Vector(n3)))
    }

    test("firstChild of a node with children is correct (s1)") {
        firstChild.unapplySeq(s1) should beSameCollectionAs(Some(Vector(v2)))
    }

    test("firstChild of a node with children is correct (s3)") {
        firstChild.unapplySeq(s3) should beSameCollectionAs(Some(Vector(v3)))
    }

    test("firstChild of a node with children is correct (s4)") {
        firstChild.unapplySeq(s4) should beSameCollectionAs(Some(Vector(e4)))
    }

    test("firstChild of a node with a list component is correct") {
        firstChild.unapplySeq(p) should beSameCollectionAs(Some(Vector(s1)))
    }

    test("firstChild of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            firstChild.unapplySeq(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // index

    test("index of root is zero (p)") {
        index(p) shouldBe 0
    }

    test("index of first child is zero (n1)") {
        index(n1) shouldBe 0
    }

    test("index of first child is zero (e1)") {
        index(e1) shouldBe 0
    }

    test("index of first child is zero (n3)") {
        index(n3) shouldBe 0
    }

    test("index of first child is zero (v2)") {
        index(v2) shouldBe 0
    }

    test("index of first child is zero (v3)") {
        index(v3) shouldBe 0
    }

    test("index of first child is zero (e4)") {
        index(e4) shouldBe 0
    }

    test("index of first child is zero (s1)") {
        index(s1) shouldBe 0
    }

    test("index of second child is one (n2)") {
        index(n2) shouldBe 1
    }

    test("index of second child is one (v1)") {
        index(v1) shouldBe 1
    }

    test("index of second child is one (e2)") {
        index(e2) shouldBe 1
    }

    test("index of second child is one (e3)") {
        index(e3) shouldBe 1
    }

    test("index of second child is one (s3)") {
        index(s3) shouldBe 1
    }

    test("index of second child is one (s2)") {
        index(s2) shouldBe 1
    }

    test("index of third child is two (s4)") {
        index(s4) shouldBe 2
    }

    test("index of fourth child is three (s5)") {
        index(s5) shouldBe 3
    }

    for (i <- 0 to nulls.size - 1) {
        test("index of nulls(" + i + ") is " + (i + 4)) {
            index(nulls(i)) shouldBe i + 4
        }
    }

    test("index of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            index(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // indexFromEnd

    test("indexFromEnd of root is zero (p)") {
        indexFromEnd(p) shouldBe 0
    }

    test("indexFromEnd of second-last child is one (n1)") {
        indexFromEnd(n1) shouldBe 1
    }

    test("indexFromEnd of second-last child is one (e1)") {
        indexFromEnd(e1) shouldBe 1
    }

    test("indexFromEnd of last child is zero (n3)") {
        indexFromEnd(n3) shouldBe 0
    }

    test("indexFromEnd of second-last child is one (v2)") {
        indexFromEnd(v2) shouldBe 1
    }

    test("indexFromEnd of second-last child is one (v3)") {
        indexFromEnd(v3) shouldBe 1
    }

    test("indexFromEnd of second-last child is one (e4)") {
        indexFromEnd(e4) shouldBe 1
    }

    test("indexFromEnd of first sequence child is twenty-seven (s1)") {
        indexFromEnd(s1) shouldBe 27
    }

    test("indexFromEnd of last child is zero (n2)") {
        indexFromEnd(n2) shouldBe 0
    }

    test("indexFromEnd of last child is zero (v1)") {
        indexFromEnd(v1) shouldBe 0
    }

    test("indexFromEnd of last child is zero (e2)") {
        indexFromEnd(e2) shouldBe 0
    }

    test("indexFromEnd of last child is zero (e3)") {
        indexFromEnd(e3) shouldBe 0
    }

    test("indexFromEnd of last child is zero (s3)") {
        indexFromEnd(s3) shouldBe 0
    }

    test("indexFromEnd of second sequence child is twenty-six (s2)") {
        indexFromEnd(s2) shouldBe 26
    }

    test("indexFromEnd of third sequence child is twenty-five (s4)") {
        indexFromEnd(s4) shouldBe 25
    }

    test("indexFromEnd of fourth sequence child is twenty-four (s5)") {
        indexFromEnd(s5) shouldBe 24
    }

    for (i <- 0 to nulls.size - 1) {
        test("indexFromEnd of nulls(" + i + ") is " + (23 - i)) {
            indexFromEnd(nulls(i)) shouldBe 23 - i
        }
    }

    test("indexFromEnd of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            indexFromEnd(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // isLeaf

    test("isLeaf returns true for a leaf") {
        isLeaf(n1) shouldBe true
    }

    test("isLeaf returns false for a non-leaf") {
        !isLeaf(e1) shouldBe true
    }

    // isFirst

    test("isFirst returns true for root") {
        isFirst(p) shouldBe true
    }

    test("isFirst returns true for a first child") {
        isFirst(n1) shouldBe true
    }

    test("isFirst returns false for a non-first child") {
        !isFirst(e3) shouldBe true
    }

    test("isFirst throws an exception for an unrelated node") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            isFirst(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // isLast

    test("isLast returns true for root") {
        isLast(p) shouldBe true
    }

    test("isLast returns true for a last child") {
        isLast(v1) shouldBe true
    }

    test("isLast returns false for a non-last child") {
        !isLast(e4) shouldBe true
    }

    test("isLast throws an exception for an unrelated node") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            isLast(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // isRoot

    test("isRoot returns true for the root") {
        isRoot(p) shouldBe true
    }

    test("isRoot returns false for a non-root node") {
        !isRoot(s3) shouldBe true
    }

    test("isRoot throws an exception for an unrelated node") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            isRoot(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // lastChild

    test("lastChild of a leaf is not defined (n1)") {
        lastChild.unapplySeq(n1) shouldBe Some(Vector())
    }

    test("lastChild of a leaf is not defined (n2)") {
        lastChild.unapplySeq(n2) shouldBe Some(Vector())
    }

    test("lastChild of a leaf is not defined (n3)") {
        lastChild.unapplySeq(n3) shouldBe Some(Vector())
    }

    test("lastChild of a leaf is not defined (v1)") {
        lastChild.unapplySeq(v1) shouldBe Some(Vector())
    }

    test("lastChild of a leaf is not defined (v2)") {
        lastChild.unapplySeq(v2) shouldBe Some(Vector())
    }

    test("lastChild of a leaf is not defined (v3)") {
        lastChild.unapplySeq(v3) shouldBe Some(Vector())
    }

    test("lastChild of a leaf is not defined (e4)") {
        lastChild.unapplySeq(e4) shouldBe Some(Vector())
    }

    test("lastChild of a leaf is not defined (s2)") {
        lastChild.unapplySeq(s2) shouldBe Some(Vector())
    }

    test("lastChild of a node with children is correct (e1)") {
        lastChild.unapplySeq(e1) should beSameCollectionAs(Some(Vector(n2)))
    }

    test("lastChild of a node with children is correct (e2)") {
        lastChild.unapplySeq(e2) should beSameCollectionAs(Some(Vector(v1)))
    }

    test("lastChild of a node with children is correct (e3)") {
        lastChild.unapplySeq(e3) should beSameCollectionAs(Some(Vector(n3)))
    }

    test("lastChild of a node with children is correct (s1)") {
        lastChild.unapplySeq(s1) should beSameCollectionAs(Some(Vector(e2)))
    }

    test("lastChild of a node with children is correct (s3)") {
        lastChild.unapplySeq(s3) should beSameCollectionAs(Some(Vector(e3)))
    }

    test("lastChild of a node with children is correct (s4)") {
        lastChild.unapplySeq(s4) should beSameCollectionAs(Some(Vector(s3)))
    }

    test("lastChild of a node with a list component is correct") {
        lastChild.unapplySeq(p) should beSameCollectionAs(Some(Vector(nulls(23))))
    }

    test("lastChild of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            lastChild.unapplySeq(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // next

    test("next of root is not defined") {
        next.image(p) shouldBe empty
    }

    test("next of a last child is not defined (n2)") {
        next.image(n2) shouldBe empty
    }

    test("next of a last child is not defined (v1)") {
        next.image(v1) shouldBe empty
    }

    test("next of a last child is not defined (n3)") {
        next.image(n3) shouldBe empty
    }

    test("next of a last child is not defined (e2)") {
        next.image(e2) shouldBe empty
    }

    test("next of a last child is not defined (e3)") {
        next.image(e3) shouldBe empty
    }

    test("next of a last child is not defined (s3)") {
        next.image(s3) shouldBe empty
    }

    test("next of a last child is not defined (nulls(23))") {
        next.image(nulls(23)) shouldBe empty
    }

    test("next of a non-last child is correct (n1)") {
        next.image(n1) should beSameCollectionAs(Vector(n2))
    }

    test("next of a non-last child is correct (e1)") {
        next.image(e1) should beSameCollectionAs(Vector(v1))
    }

    test("next of a non-last child is correct (v2)") {
        next.image(v2) should beSameCollectionAs(Vector(e2))
    }

    test("next of a non-last child is correct (v3)") {
        next.image(v3) should beSameCollectionAs(Vector(e3))
    }

    test("next of a non-last child is correct (e4)") {
        next.image(e4) should beSameCollectionAs(Vector(s3))
    }

    test("next of a non-last child is correct (s1)") {
        next.image(s1) should beSameCollectionAs(Vector(s2))
    }

    test("next of a non-last child is correct (s2)") {
        next.image(s2) should beSameCollectionAs(Vector(s4))
    }

    test("next of a non-last child is correct (s4)") {
        next.image(s4) should beSameCollectionAs(Vector(s5))
    }

    for (i <- 0 to nulls.size - 2) {
        test("next of nulls(" + i + ") is nulls(" + (i + 1) + ")") {
            next.image(nulls(i)) should beSameCollectionAs(Vector(nulls(i + 1)))
        }
    }

    test("next of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            next(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // parent

    test("parent of root is not defined") {
        parent.image(p) shouldBe empty
    }

    test("parent of leaf is its parent (n1)") {
        parent.image(n1) should beSameCollectionAs(Vector(e1))
    }

    test("parent of leaf is its parent (n2)") {
        parent.image(n2) should beSameCollectionAs(Vector(e1))
    }

    test("parent of leaf is its parent (n3)") {
        parent.image(n3) should beSameCollectionAs(Vector(e3))
    }

    test("parent of leaf is its parent (v1)") {
        parent.image(v1) should beSameCollectionAs(Vector(e2))
    }

    test("parent of leaf is its parent (v2)") {
        parent.image(v2) should beSameCollectionAs(Vector(s1))
    }

    test("parent of leaf is its parent (v3)") {
        parent.image(v3) should beSameCollectionAs(Vector(s3))
    }

    test("parent of leaf is its parent (e4)") {
        parent.image(e4) should beSameCollectionAs(Vector(s4))
    }

    test("parent of interior node is its parent (e1)") {
        parent.image(e1) should beSameCollectionAs(Vector(e2))
    }

    test("parent of interior node is its parent (e2)") {
        parent.image(e2) should beSameCollectionAs(Vector(s1))
    }

    test("parent of interior node is its parent (e3)") {
        parent.image(e3) should beSameCollectionAs(Vector(s3))
    }

    test("parent of interior node is its parent (e4)") {
        parent.image(e4) should beSameCollectionAs(Vector(s4))
    }

    test("parent of interior node is its parent (s3)") {
        parent.image(s3) should beSameCollectionAs(Vector(s4))
    }

    test("parent of node in Program is the program (s1)") {
        parent.image(s1) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Program is the program (s2)") {
        parent.image(s2) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Program is the program (s4)") {
        parent.image(s4) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Program is the program (s5)") {
        parent.image(s5) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Option field of Program is the program") {
        parent.image(nulls(0)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node after Option field of Program is the program") {
        parent.image(nulls(1)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Left field of Program is the program") {
        parent.image(nulls(2)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Right field of Program is the program") {
        parent.image(nulls(3)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node after Either fields of Program is the program") {
        parent.image(nulls(4)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 1 of Program is the program") {
        parent.image(nulls(5)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 2 of Program is the program (first)") {
        parent.image(nulls(6)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 2 of Program is the program (second)") {
        parent.image(nulls(7)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 3 of Program is the program (first)") {
        parent.image(nulls(8)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 3 of Program is the program (second)") {
        parent.image(nulls(9)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 3 of Program is the program (third)") {
        parent.image(nulls(10)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 4 of Program is the program (first)") {
        parent.image(nulls(11)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 4 of Program is the program (second)") {
        parent.image(nulls(12)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 4 of Program is the program (third)") {
        parent.image(nulls(13)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in tuple 4 of Program is the program (fourth)") {
        parent.image(nulls(14)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node after tuple 4 field of Program is the program") {
        parent.image(nulls(15)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in list of Somes of Program is the program (first)") {
        parent.image(nulls(16)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in list of Somes of Program is the program (second)") {
        parent.image(nulls(17)) should beSameCollectionAs(Vector(p))
    }

    test("parent of middle field of Program is the program") {
        parent.image(nulls(18)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Vector field of Program is the program (first)") {
        parent.image(nulls(19)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Vector field of Program is the program (second)") {
        parent.image(nulls(20)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Vector field of Program is the program (third)") {
        parent.image(nulls(21)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Map field of Program is the program (first)") {
        parent.image(nulls(22)) should beSameCollectionAs(Vector(p))
    }

    test("parent of node in Map field of Program is the program (second)") {
        parent.image(nulls(23)) should beSameCollectionAs(Vector(p))
    }

    test("parent of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            parent(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // prev

    test("prev of root is not defined") {
        prev.image(p) shouldBe empty
    }

    test("prev of a first child is not defined (n1)") {
        prev.image(n1) shouldBe empty
    }

    test("prev of a first child is not defined (e1)") {
        prev.image(e1) shouldBe empty
    }

    test("prev of a first child is not defined (n3)") {
        prev.image(n3) shouldBe empty
    }

    test("prev of a first child is not defined (v2)") {
        prev.image(v2) shouldBe empty
    }

    test("prev of a first child is not defined (v3)") {
        prev.image(v3) shouldBe empty
    }

    test("prev of a first child is not defined (e4)") {
        prev.image(e4) shouldBe empty
    }

    test("prev of a first child is not defined (s1)") {
        prev.image(s1) shouldBe empty
    }

    test("prev of a non-first child is correct (n2)") {
        prev.image(n2) should beSameCollectionAs(Vector(n1))
    }

    test("prev of a non-first child is correct (v1)") {
        prev.image(v1) should beSameCollectionAs(Vector(e1))
    }

    test("prev of a non-first child is correct (e2)") {
        prev.image(e2) should beSameCollectionAs(Vector(v2))
    }

    test("prev of a non-first child is correct (e3)") {
        prev.image(e3) should beSameCollectionAs(Vector(v3))
    }

    test("prev of a non-first child is correct (s3)") {
        prev.image(s3) should beSameCollectionAs(Vector(e4))
    }

    test("prev of a non-first child is correct (s2)") {
        prev.image(s2) should beSameCollectionAs(Vector(s1))
    }

    test("prev of a non-first child is correct (s4)") {
        prev.image(s4) should beSameCollectionAs(Vector(s2))
    }

    test("prev of a non-first child is correct (s5)") {
        prev.image(s5) should beSameCollectionAs(Vector(s4))
    }

    test("prev of a nulls(0) is correct") {
        prev.image(nulls(0)) should beSameCollectionAs(Vector(s5))
    }

    for (i <- 1 to nulls.size - 1) {
        test("prev of nulls(" + i + ") is nulls(" + (i - 1) + ")") {
            prev.image(nulls(i)) should beSameCollectionAs(Vector(nulls(i - 1)))
        }
    }

    test("prev of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            prev(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // sibling

    test("root has itself as a sibling (sibling relation)") {
        sibling.image(p) should beSameCollectionAs(Vector(p))
    }

    test("an only child has itself as a sibling (sibling relation) (n3)") {
        sibling.image(n3) should beSameCollectionAs(Vector(n3))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (n1)") {
        sibling.image(n1) should beSameCollectionAs(Vector(n1, n2))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (n2)") {
        sibling.image(n2) should beSameCollectionAs(Vector(n1, n2))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (e1)") {
        sibling.image(e1) should beSameCollectionAs(Vector(e1, v1))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (e2)") {
        sibling.image(e2) should beSameCollectionAs(Vector(v2, e2))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (v2)") {
        sibling.image(v2) should beSameCollectionAs(Vector(v2, e2))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (e3)") {
        sibling.image(e3) should beSameCollectionAs(Vector(v3, e3))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (v3)") {
        sibling.image(v3) should beSameCollectionAs(Vector(v3, e3))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (e4)") {
        sibling.image(e4) should beSameCollectionAs(Vector(e4, s3))
    }

    test("a child of a normal node has the expected siblings (sibling relation) (s3)") {
        sibling.image(s3) should beSameCollectionAs(Vector(e4, s3))
    }

    test("a child of a node with a list component has the expected siblings (sibling relation) (s1") {
        sibling.image(s1) should beSameCollectionAs(pchildren)
    }

    test("a child of a node with a list component has the expected siblings (sibling relation) (s2)") {
        sibling.image(s2) should beSameCollectionAs(pchildren)
    }

    test("a child of a node with a list component has the expected siblings (sibling relation) (sibling relation) (s4)") {
        sibling.image(s4) should beSameCollectionAs(pchildren)
    }

    test("a child of a node with a list component has the expected siblings (sibling relation) (s5)") {
        sibling.image(s5) should beSameCollectionAs(pchildren)
    }

    test("sibling of non-node throws an exception (sibling relation)") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            sibling.image(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // siblings

    test("root has itself as a sibling (siblings method)") {
        siblings(p) should beSameCollectionAs(Vector(p))
    }

    test("an only child has itself as a sibling (siblings method) (n3)") {
        siblings(n3) should beSameCollectionAs(Vector(n3))
    }

    test("a child of a normal node has the expected siblings (siblings method) (n1)") {
        siblings(n1) should beSameCollectionAs(Vector(n1, n2))
    }

    test("a child of a normal node has the expected siblings (siblings method) (n2)") {
        siblings(n2) should beSameCollectionAs(Vector(n1, n2))
    }

    test("a child of a normal node has the expected siblings (siblings method) (e1)") {
        siblings(e1) should beSameCollectionAs(Vector(e1, v1))
    }

    test("a child of a normal node has the expected siblings (siblings method) (e2)") {
        siblings(e2) should beSameCollectionAs(Vector(v2, e2))
    }

    test("a child of a normal node has the expected siblings (siblings method) (v2)") {
        siblings(v2) should beSameCollectionAs(Vector(v2, e2))
    }

    test("a child of a normal node has the expected siblings (siblings method) (e3)") {
        siblings(e3) should beSameCollectionAs(Vector(v3, e3))
    }

    test("a child of a normal node has the expected siblings (siblings method) (v3)") {
        siblings(v3) should beSameCollectionAs(Vector(v3, e3))
    }

    test("a child of a normal node has the expected siblings (siblings method) (e4)") {
        siblings(e4) should beSameCollectionAs(Vector(e4, s3))
    }

    test("a child of a normal node has the expected siblings (siblings method) (s3)") {
        siblings(s3) should beSameCollectionAs(Vector(e4, s3))
    }

    test("a child of a node with a list component has the expected siblings (siblings method) (s1") {
        siblings(s1) should beSameCollectionAs(pchildren)
    }

    test("a child of a node with a list component has the expected siblings (siblings method) (s2)") {
        siblings(s2) should beSameCollectionAs(pchildren)
    }

    test("a child of a node with a list component has the expected siblings (siblings method) (s4)") {
        siblings(s4) should beSameCollectionAs(pchildren)
    }

    test("a child of a node with a list component has the expected siblings (siblings method) (s5)") {
        siblings(s5) should beSameCollectionAs(pchildren)
    }

    test("siblings of non-node throws an exception (siblings method)") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            siblings(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

    // sibling count

    test("root has one sibling") {
        siblingCount(p) shouldBe 1
    }
    test("an only child has one sibling (n3)") {
        siblingCount(n3) shouldBe 1
    }

    test("a child of a normal node has the expected number of siblings (n1)") {
        siblingCount(n1) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (n2)") {
        siblingCount(n2) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (e1)") {
        siblingCount(e1) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (e2)") {
        siblingCount(e2) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (v2)") {
        siblingCount(v2) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (e3)") {
        siblingCount(e3) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (v3)") {
        siblingCount(v3) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (e4)") {
        siblingCount(e4) shouldBe 2
    }

    test("a child of a normal node has the expected number of siblings (s3)") {
        siblingCount(s3) shouldBe 2
    }

    test("a child of a node with a list component has the expected number of siblings (s1)") {
        siblingCount(s1) shouldBe (pchildren.length)
    }

    test("a child of a node with a list component has the expected number of siblings (s2)") {
        siblingCount(s2) shouldBe (pchildren.length)
    }

    test("a child of a node with a list component has the expected number of siblings (s4)") {
        siblingCount(s4) shouldBe (pchildren.length)
    }

    test("a child of a node with a list component has the expected number of siblings (s5)") {
        siblingCount(s5) shouldBe (pchildren.length)
    }

    test("sibling count of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            siblingCount(nonNode)
        }
        i.getMessage shouldBe "node not in tree: Num(1.0)"
    }

}
