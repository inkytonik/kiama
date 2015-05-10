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

import org.kiama.util.Tests

/**
 * Tests of tree relations.
 */
class TreeTests extends Tests with RelationTestSupport {

    import org.kiama.example.imperative.ImperativeTree._
    import org.kiama.relation.Tree.isLeaf
    import scala.collection.immutable.Set

    // Test tree

    val n1 = Num (1) // these are deliberately ==, but not same
    val n2 = Num (1)
    val n3 = Num (1)
    val n4 = Num (1)

    val v1 = Var ("a")
    val v2 = Var ("b")
    val v3 = Var ("c")

    val e1 = Add (n1, n2)
    val e2 = Mul (e1, v1)
    val e3 = Neg (n3)
    val e4 = Var ("d")

    val s1 = Asgn (v2, e2)
    val s2 = Null ()
    val s3 = Asgn (v3, e3)
    val s4 = While (e4, s3)
    val s5 = Null ()

    val nulls = Vector.fill (24) (Null ())

    case class Program (u : Stmt, as : List[Stmt], v : Stmt, optb1 : Option[Stmt],
                        optb2 : Option[Stmt], w : Stmt, eitherc1 : Either[Stmt,Stmt],
                        eitherc2 : Either[Stmt,Stmt], x : Stmt,
                        t1 : Tuple1[Stmt], t2 : (Stmt,Stmt), t3 : (Stmt,Stmt,Stmt),
                        t4 : (Stmt,Stmt,Stmt,Stmt),
                        y : Stmt, listopts : List[Option[Stmt]], z : Stmt,
                        vec : Vector[Stmt], map : Map[Int,Stmt],
                        cross : Bridge[Num]) extends ImperativeNode

    val p = Program (s1, List (s2, s4), s5, Some (nulls (0)), None, nulls (1),
                     Left (nulls (2)), Right (nulls (3)), nulls (4),
                     Tuple1 (nulls(5)), (nulls (6), nulls (7)),
                     (nulls (8), nulls (9), nulls (10)),
                     (nulls (11), nulls (12), nulls (13), nulls (14)),
                     nulls (15), List (Some (nulls (16)), Some (nulls (17))), nulls (18),
                     Vector (nulls (19), nulls (20), nulls (21)),
                     Map (1 ->  nulls (22), 2 -> nulls (23)),
                     Bridge (n4))

    val pchildren = List (s1, s2, s4, s5) ++ nulls

    val ptree = new ImperativeTree (p)
    import ptree._

    // A value that is not a node of the `p` tree
    val nonNode = Num (1)

    // child

    test ("child of leaf is empty (n1)") {
        assertImage (child, n1)
    }

    test ("child of leaf is empty (n2)") {
        assertImage (child, n2)
    }

    test ("child of leaf is empty (n3)") {
        assertImage (child, n3)
    }

    test ("child of leaf is empty (v1)") {
        assertImage (child, v1)
    }

    test ("child of leaf is empty (v2)") {
        assertImage (child, v2)
    }

    test ("child of leaf is empty (v3)") {
        assertImage (child, v3)
    }

    test ("child of leaf is empty (e4)") {
        assertImage (child, v1)
    }

    test ("child of leaf is empty (s2)") {
        assertImage (child, s2)
    }

    test ("child of interior node is its children (e1)") {
        assertImage (child, e1, List (n1, n2))
    }

    test ("child of interior node is its children (e2)") {
        assertImage (child, e2, List (e1, v1))
    }

    test ("child of interior node is its children (e3)") {
        assertImage (child, e3, List (n3))
    }

    test ("child of interior node is its children (s1)") {
        assertImage (child, s1, List (v2, e2))
    }

    test ("child of interior node is its children (v3)") {
        assertImage (child, s3, List (v3, e3))
    }

    test ("child of interior node is its children (s4)") {
        assertImage (child, s4, List (e4, s3))
    }

    test ("child of Program ignores its non ImperativeTree fields") {
        assertImage (child, p, pchildren)
    }

    test ("child of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.child (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // firstChild

    test ("firstChild of a leaf is not defined (n1)") {
        assertResult (None) (firstChild.unapply (n1))
    }

    test ("firstChild of a leaf is not defined (n2)") {
        assertResult (None) (firstChild.unapply (n2))
    }

    test ("firstChild of a leaf is not defined (n3)") {
        assertResult (None) (firstChild.unapply (n3))
    }

    test ("firstChild of a leaf is not defined (v1)") {
        assertResult (None) (firstChild.unapply (v1))
    }

    test ("firstChild of a leaf is not defined (v2)") {
        assertResult (None) (firstChild.unapply (v2))
    }

    test ("firstChild of a leaf is not defined (v3)") {
        assertResult (None) (firstChild.unapply (v3))
    }

    test ("firstChild of a leaf is not defined (e4)") {
        assertResult (None) (firstChild.unapply (e4))
    }

    test ("firstChild of a leaf is not defined (s2)") {
        assertResult (None) (firstChild.unapply (s2))
    }

    test ("firstChild of a node with children is correct (e1)") {
        assertResult (Some (n1)) (firstChild.unapply (e1))
    }

    test ("firstChild of a node with children is correct (e2)") {
        assertResult (Some (e1)) (firstChild.unapply (e2))
    }

    test ("firstChild of a node with children is correct (e3)") {
        assertResult (Some (n3)) (firstChild.unapply (e3))
    }

    test ("firstChild of a node with children is correct (s1)") {
        assertResult (Some (v2)) (firstChild.unapply (s1))
    }

    test ("firstChild of a node with children is correct (s3)") {
        assertResult (Some (v3)) (firstChild.unapply (s3))
    }

    test ("firstChild of a node with children is correct (s4)") {
        assertResult (Some (e4)) (firstChild.unapply (s4))
    }

    test ("firstChild of a node with a list component is correct") {
        assertResult (Some (s1)) (firstChild.unapply (p))
    }

    test ("firstChild of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            firstChild.unapply (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // index

    test ("index of root is zero (p)") {
        assertResult (0) (index (p))
    }

    test ("index of first child is zero (n1)") {
        assertResult (0) (index (n1))
    }

    test ("index of first child is zero (e1)") {
        assertResult (0) (index (e1))
    }

    test ("index of first child is zero (n3)") {
        assertResult (0) (index (n3))
    }

    test ("index of first child is zero (v2)") {
        assertResult (0) (index (v2))
    }

    test ("index of first child is zero (v3)") {
        assertResult (0) (index (v3))
    }

    test ("index of first child is zero (e4)") {
        assertResult (0) (index (e4))
    }

    test ("index of first child is zero (s1)") {
        assertResult (0) (index (s1))
    }

    test ("index of second child is one (n2)") {
        assertResult (1) (index (n2))
    }

    test ("index of second child is one (v1)") {
        assertResult (1) (index (v1))
    }

    test ("index of second child is one (e2)") {
        assertResult (1) (index (e2))
    }

    test ("index of second child is one (e3)") {
        assertResult (1) (index (e3))
    }

    test ("index of second child is one (s3)") {
        assertResult (1) (index (s3))
    }

    test ("index of second child is one (s2)") {
        assertResult (1) (index (s2))
    }

    test ("index of third child is two (s4)") {
        assertResult (2) (index (s4))
    }

    test ("index of fourth child is three (s5)") {
        assertResult (3) (index (s5))
    }

    for (i <- 0 to nulls.size - 1) {
        test ("index of nulls (" + i + ") is " + (i + 4)) {
            assertResult (i + 4) (index (nulls (i)))
        }
    }

    test ("index of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.index (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // isLeaf

    test ("isLeaf returns true for a leaf") {
        assert (isLeaf (n1))
    }

    test ("isLeaf returns false for a non-leaf") {
        assert (!isLeaf (e1))
    }

    // isFirst

    test ("isFirst returns true for root") {
        assert (ptree.isFirst (p))
    }

    test ("isFirst returns true for a first child") {
        assert (ptree.isFirst (n1))
    }

    test ("isFirst returns false for a non-first child") {
        assert (!ptree.isFirst (e3))
    }

    test ("isFirst throws an exception for an unrelated node") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.isFirst (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // isLast

    test ("isLast returns true for root") {
        assert (ptree.isLast (p))
    }

    test ("isLast returns true for a last child") {
        assert (ptree.isLast (v1))
    }

    test ("isLast returns false for a non-last child") {
        assert (!ptree.isLast (e4))
    }

    test ("isLast throws an exception for an unrelated node") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.isLast (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // isRoot

    test ("isRoot returns true for the root") {
        assert (ptree.isRoot (p))
    }

    test ("isRoot returns false for a non-root node") {
        assert (!ptree.isRoot (s3))
    }

    test ("isRoot throws an exception for an unrelated node") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.isRoot (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // lastChild

    test ("lastChild of a leaf is not defined (n1)") {
        assertResult (None) (lastChild.unapply (n1))
    }

    test ("lastChild of a leaf is not defined (n2)") {
        assertResult (None) (lastChild.unapply (n2))
    }

    test ("lastChild of a leaf is not defined (n3)") {
        assertResult (None) (lastChild.unapply (n3))
    }

    test ("lastChild of a leaf is not defined (v1)") {
        assertResult (None) (lastChild.unapply (v1))
    }

    test ("lastChild of a leaf is not defined (v2)") {
        assertResult (None) (lastChild.unapply (v2))
    }

    test ("lastChild of a leaf is not defined (v3)") {
        assertResult (None) (lastChild.unapply (v3))
    }

    test ("lastChild of a leaf is not defined (e4)") {
        assertResult (None) (lastChild.unapply (e4))
    }

    test ("lastChild of a leaf is not defined (s2)") {
        assertResult (None) (lastChild.unapply (s2))
    }

    test ("lastChild of a node with children is correct (e1)") {
        assertResult (Some (n2)) (lastChild.unapply (e1))
    }

    test ("lastChild of a node with children is correct (e2)") {
        assertResult (Some (v1)) (lastChild.unapply (e2))
    }

    test ("lastChild of a node with children is correct (e3)") {
        assertResult (Some (n3)) (lastChild.unapply (e3))
    }

    test ("lastChild of a node with children is correct (s1)") {
        assertResult (Some (e2)) (lastChild.unapply (s1))
    }

    test ("lastChild of a node with children is correct (s3)") {
        assertResult (Some (e3)) (lastChild.unapply (s3))
    }

    test ("lastChild of a node with children is correct (s4)") {
        assertResult (Some (s3)) (lastChild.unapply (s4))
    }

    test ("lastChild of a node with a list component is correct") {
        assertResult (Some (nulls (17))) (lastChild.unapply (p))
    }

    test ("lastChild of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            lastChild.unapply (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // next

    test ("next of root is not defined") {
        assertImage (next, p)
    }

    test ("next of a last child is not defined (n2)") {
        assertImage (next, n2)
    }

    test ("next of a last child is not defined (v1)") {
        assertImage (next, v1)
    }

    test ("next of a last child is not defined (n3)") {
        assertImage (next, n3)
    }

    test ("next of a last child is not defined (e2)") {
        assertImage (next, e2)
    }

    test ("next of a last child is not defined (e3)") {
        assertImage (next, e3)
    }

    test ("next of a last child is not defined (s3)") {
        assertImage (next, s3)
    }

    test ("next of a last child is not defined (nulls (23))") {
        assertImage (next, nulls (23))
    }

    test ("next of a non-last child is correct (n1)") {
        assertImage (next, n1, List (n2))
    }

    test ("next of a non-last child is correct (e1)") {
        assertImage (next, e1, List (v1))
    }

    test ("next of a non-last child is correct (v2)") {
        assertImage (next, v2, List (e2))
    }

    test ("next of a non-last child is correct (v3)") {
        assertImage (next, v3, List (e3))
    }

    test ("next of a non-last child is correct (e4)") {
        assertImage (next, e4, List (s3))
    }

    test ("next of a non-last child is correct (s1)") {
        assertImage (next, s1, List (s2))
    }

    test ("next of a non-last child is correct (s2)") {
        assertImage (next, s2, List (s4))
    }

    test ("next of a non-last child is correct (s4)") {
        assertImage (next, s4, List (s5))
    }

    for (i <- 0 to nulls.size - 2) {
        test ("next of nulls (" + i + ") is nulls (" + (i + 1) + ")") {
            assertImage (next, nulls (i), List (nulls (i + 1)))
        }
    }

    test ("next of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.next (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // parent

    test ("parent of root is not defined") {
        assertImage (parent, p)
    }

    test ("parent of leaf is its parent (n1)") {
        assertImage (parent, n1, List (e1))
    }

    test ("parent of leaf is its parent (n2)") {
        assertImage (parent, n2, List (e1))
    }

    test ("parent of leaf is its parent (n3)") {
        assertImage (parent, n3, List (e3))
    }

    test ("parent of leaf is its parent (v1)") {
        assertImage (parent, v1, List (e2))
    }

    test ("parent of leaf is its parent (v2)") {
        assertImage (parent, v2, List (s1))
    }

    test ("parent of leaf is its parent (v3)") {
        assertImage (parent, v3, List (s3))
    }

    test ("parent of leaf is its parent (e4)") {
        assertImage (parent, e4, List (s4))
    }

    test ("parent of interior node is its parent (e1)") {
        assertImage (parent, e1, List (e2))
    }

    test ("parent of interior node is its parent (e2)") {
        assertImage (parent, e2, List (s1))
    }

    test ("parent of interior node is its parent (e3)") {
        assertImage (parent, e3, List (s3))
    }

    test ("parent of interior node is its parent (e4)") {
        assertImage (parent, e4, List (s4))
    }

    test ("parent of interior node is its parent (s3)") {
        assertImage (parent, s3, List (s4))
    }

    test ("parent of node in Program is the program (s1)") {
        assertImage (parent, s1, List (p))
    }

    test ("parent of node in Program is the program (s2)") {
        assertImage (parent, s2, List (p))
    }

    test ("parent of node in Program is the program (s4)") {
        assertImage (parent, s4, List (p))
    }

    test ("parent of node in Program is the program (s5)") {
        assertImage (parent, s5, List (p))
    }

    test ("parent of node in Option field of Program is the program") {
        assertImage (parent, nulls (0), List (p))
    }

    test ("parent of node after Option field of Program is the program") {
        assertImage (parent, nulls (1), List (p))
    }

    test ("parent of node in Left field of Program is the program") {
        assertImage (parent, nulls (2), List (p))
    }

    test ("parent of node in Right field of Program is the program") {
        assertImage (parent, nulls (3), List (p))
    }

    test ("parent of node after Either fields of Program is the program") {
        assertImage (parent, nulls (4), List (p))
    }

    test ("parent of node in tuple 1 of Program is the program") {
        assertImage (parent, nulls (5), List (p))
    }

    test ("parent of node in tuple 2 of Program is the program (first)") {
        assertImage (parent, nulls (6), List (p))
    }

    test ("parent of node in tuple 2 of Program is the program (second)") {
        assertImage (parent, nulls (7), List (p))
    }

    test ("parent of node in tuple 3 of Program is the program (first)") {
        assertImage (parent, nulls (8), List (p))
    }

    test ("parent of node in tuple 3 of Program is the program (second)") {
        assertImage (parent, nulls (9), List (p))
    }

    test ("parent of node in tuple 3 of Program is the program (third)") {
        assertImage (parent, nulls (10), List (p))
    }

    test ("parent of node in tuple 4 of Program is the program (first)") {
        assertImage (parent, nulls (11), List (p))
    }

    test ("parent of node in tuple 4 of Program is the program (second)") {
        assertImage (parent, nulls (12), List (p))
    }

    test ("parent of node in tuple 4 of Program is the program (third)") {
        assertImage (parent, nulls (13), List (p))
    }

    test ("parent of node in tuple 4 of Program is the program (fourth)") {
        assertImage (parent, nulls (14), List (p))
    }

    test ("parent of node after tuple 4 field of Program is the program") {
        assertImage (parent, nulls (15), List (p))
    }

    test ("parent of node in list of Somes of Program is the program (first)") {
        assertImage (parent, nulls (16), List (p))
    }

    test ("parent of node in list of Somes of Program is the program (second)") {
        assertImage (parent, nulls (17), List (p))
    }

    test ("parent of middle field of Program is the program") {
        assertImage (parent, nulls (18), List (p))
    }

    test ("parent of node in Vector field of Program is the program (first)") {
        assertImage (parent, nulls (19), List (p))
    }

    test ("parent of node in Vector field of Program is the program (second)") {
        assertImage (parent, nulls (20), List (p))
    }

    test ("parent of node in Vector field of Program is the program (third)") {
        assertImage (parent, nulls (21), List (p))
    }

    test ("parent of node in Map field of Program is the program (first)") {
        assertImage (parent, nulls (22), List (p))
    }

    test ("parent of node in Map field of Program is the program (second)") {
        assertImage (parent, nulls (23), List (p))
    }

    test ("parent of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.parent (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // prev

    test ("prev of root is not defined") {
        assertImage (prev, p)
    }

    test ("prev of a first child is not defined (n1)") {
        assertImage (prev, n1)
    }

    test ("prev of a first child is not defined (e1)") {
        assertImage (prev, e1)
    }

    test ("prev of a first child is not defined (n3)") {
        assertImage (prev, n3)
    }

    test ("prev of a first child is not defined (v2)") {
        assertImage (prev, v2)
    }

    test ("prev of a first child is not defined (v3)") {
        assertImage (prev, v3)
    }

    test ("prev of a first child is not defined (e4)") {
        assertImage (prev, e4)
    }

    test ("prev of a first child is not defined (s1)") {
        assertImage (prev, s1)
    }

    test ("prev of a non-first child is correct (n2)") {
        assertImage (prev, n2, List (n1))
    }

    test ("prev of a non-first child is correct (v1)") {
        assertImage (prev, v1, List (e1))
    }

    test ("prev of a non-first child is correct (e2)") {
        assertImage (prev, e2, List (v2))
    }

    test ("prev of a non-first child is correct (e3)") {
        assertImage (prev, e3, List (v3))
    }

    test ("prev of a non-first child is correct (s3)") {
        assertImage (prev, s3, List (e4))
    }

    test ("prev of a non-first child is correct (s2)") {
        assertImage (prev, s2, List (s1))
    }

    test ("prev of a non-first child is correct (s4)") {
        assertImage (prev, s4, List (s2))
    }

    test ("prev of a non-first child is correct (s5)") {
        assertImage (prev, s5, List (s4))
    }

    test ("prev of a nulls (0) is correct") {
        assertImage (prev, nulls (0), List (s5))
    }

    for (i <- 1 to nulls.size - 1) {
        test ("prev of nulls (" + i + ") is nulls (" + (i - 1) + ")") {
            assertImage (prev, nulls (i), List (nulls (i - 1)))
        }
    }

    test ("prev of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.next (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

    // siblings

    test ("root has itself as a sibling") {
        assertImage (siblings, p, List (p))
    }

    test ("an only child has itself as a sibling (n3)") {
        assertImage (siblings, n3, List (n3))
    }

    test ("a child of a normal node has the expected siblings (n1)") {
        assertImage (siblings, n1, List (n1, n2))
    }

    test ("a child of a normal node has the expected siblings (n2)") {
        assertImage (siblings, n2, List (n1, n2))
    }

    test ("a child of a normal node has the expected siblings (e1)") {
        assertImage (siblings, e1, List (e1, v1))
    }

    test ("a child of a normal node has the expected siblings (e2)") {
        assertImage (siblings, e2, List (v2, e2))
    }

    test ("a child of a normal node has the expected siblings (v2)") {
        assertImage (siblings, v2, List (v2, e2))
    }

    test ("a child of a normal node has the expected siblings (e3)") {
        assertImage (siblings, e3, List (v3, e3))
    }

    test ("a child of a normal node has the expected siblings (v3)") {
        assertImage (siblings, v3, List (v3, e3))
    }

    test ("a child of a normal node has the expected siblings (e4)") {
        assertImage (siblings, e4, List (e4, s3))
    }

    test ("a child of a normal node has the expected siblings (s3)") {
        assertImage (siblings, s3, List (e4, s3))
    }

    test ("a child of a node with a list component has the expected siblings (s1") {
        assertImage (siblings, s1, pchildren)
    }

    test ("a child of a node with a list component has the expected siblings (s2)") {
        assertImage (siblings, s2, pchildren)
    }

    test ("a child of a node with a list component has the expected siblings (s4)") {
        assertImage (siblings, s4, pchildren)
    }

    test ("a child of a node with a list component has the expected siblings (s5)") {
        assertImage (siblings, s5, pchildren)
    }

    test ("siblings of non-node throws an exception") {
        val i = intercept[NodeNotInTreeException[Exp]] {
            ptree.next (nonNode)
        }
        assertResult ("node not in tree: Num(1.0)") (i.getMessage)
    }

}
