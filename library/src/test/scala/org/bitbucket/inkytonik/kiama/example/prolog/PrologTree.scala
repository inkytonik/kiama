/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.prolog

/**
 * Module containing tree structures for representing Prolog programs.
 */
object PrologTree {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    type PrologTree = Tree[PrologNode, Program]

    sealed abstract class PrologNode extends Product

    case class Program(cs : Vector[Clause]) extends PrologNode

    sealed abstract class Clause extends PrologNode {
        def hd : Term
        def bdy : Vector[Term]
    }

    case class Fact(hd : Term) extends Clause {
        def bdy : Vector[Term] = Vector()
    }
    case class Rule(hd : Term, bdy : Vector[Term]) extends Clause

    sealed abstract class Term extends PrologNode

    case class Var(s : String) extends Term {
        override def toString : String = s
    }
    case class Integer(v : Int) extends Term {
        override def toString : String = v.toString
    }

    sealed abstract class Literal extends Term

    sealed abstract class NamedLiteral extends Literal

    case class Atom(s : String) extends NamedLiteral {
        override def toString : String = s
    }
    case class Pred(s : String, ts : Vector[Term]) extends NamedLiteral {
        override def toString : String = s + ts.mkString("(", ", ", ")")
    }
    case class Cut() extends Literal {
        override def toString : String = "!"
    }

}
