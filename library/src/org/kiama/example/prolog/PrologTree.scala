/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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
package example.prolog

/**
 * Module containing tree structures for representing Prolog programs.
 */
object PrologTree {

    import org.kiama.relation.Tree
    import scala.collection.immutable.Seq

    type PrologTree = Tree[PrologNode,Program]

    sealed abstract class PrologNode extends Product

    case class Program (cs : Seq[Clause]) extends PrologNode

    sealed abstract class Clause extends PrologNode {
        def hd : Term
        def bdy : Seq[Term]
    }

    case class Fact (hd : Term) extends Clause {
        def bdy : Seq[Term] = Nil
    }
    case class Rule (hd : Term, bdy : Seq[Term]) extends Clause

    sealed abstract class Term extends PrologNode

    case class Var (s : String) extends Term {
        override def toString : String = s
    }
    case class Integer (v : Int) extends Term {
        override def toString : String = v.toString
    }

    sealed abstract class Literal extends Term

    sealed abstract class NamedLiteral extends Literal

    case class Atom (s : String) extends NamedLiteral {
        override def toString : String = s
    }
    case class Pred (s : String, ts : Seq[Term]) extends NamedLiteral {
        override def toString : String = s + ts.mkString ("(", ", ", ")")
    }
    case class Cut () extends Literal {
        override def toString : String = "!"
    }

}
