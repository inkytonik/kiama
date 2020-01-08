/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.imperative

/**
 * A version of the imperative expression abstract syntax built without
 * using case classes.  Used for testing on non-case class data structures.
 */
object ImperativeNonCaseTree {

    import org.bitbucket.inkytonik.kiama.rewriting.Rewritable
    import scala.collection.immutable.Seq

    type Idn = String

    sealed abstract class Exp extends Rewritable

    class Num(val d : Double) extends Exp {
        def arity : Int = 1
        def deconstruct : Seq[Double] = Seq(d)
        def reconstruct(cs : Seq[Any]) : Exp =
            cs match {
                case Seq(d : Double) =>
                    new Num(d)
                case _ =>
                    illegalArgs("Num", "Double", cs)
            }
        override def toString : String = s"Num($d)"
    }

    class Var(val s : Idn) extends Exp {
        def arity : Int = 1
        def deconstruct : Seq[Idn] = Seq(s)
        def reconstruct(cs : Seq[Any]) : Exp =
            cs match {
                case Seq(s : Idn) =>
                    new Var(s)
                case _ =>
                    illegalArgs("Var", "Idn", cs)
            }
        override def toString : String = s"Var($s)"
    }

    class Neg(val e : Exp) extends Exp {
        def arity : Int = 1
        def deconstruct : Seq[Exp] = Seq(e)
        def reconstruct(cs : Seq[Any]) : Exp =
            cs match {
                case Seq(e : Exp) =>
                    new Neg(e)
                case _ =>
                    illegalArgs("Neg", "Exp", cs)
            }
        override def toString : String = s"Neg($e)"
    }

    abstract class Binary(val l : Exp, val r : Exp) extends Exp {
        def arity : Int = 2
        def deconstruct : Seq[Exp] = Seq(l, r)
    }

    class Add(l : Exp, r : Exp) extends Binary(l, r) {
        def reconstruct(cs : Seq[Any]) : Exp =
            cs match {
                case Seq(l : Exp, r : Exp) =>
                    new Add(l, r)
                case _ =>
                    illegalArgs("Add", "Exp, Exp", cs)
            }
        override def toString : String = s"Add($l,$r)"
    }

    class Sub(l : Exp, r : Exp) extends Binary(l, r) {
        def reconstruct(cs : Seq[Any]) : Exp =
            cs match {
                case Seq(l : Exp, r : Exp) =>
                    new Sub(l, r)
                case _ =>
                    illegalArgs("Sub", "Exp, Exp", cs)
            }
        override def toString : String = s"Sub($l,$r)"
    }

    class Mul(l : Exp, r : Exp) extends Binary(l, r) {
        def reconstruct(cs : Seq[Any]) : Exp =
            cs match {
                case Seq(l : Exp, r : Exp) =>
                    new Mul(l, r)
                case _ =>
                    illegalArgs("Mul", "Exp, Exp", cs)
            }
        override def toString : String = s"Mul($l,$r)"
    }

    class Div(l : Exp, r : Exp) extends Binary(l, r) {
        def reconstruct(cs : Seq[Any]) : Exp =
            cs match {
                case Seq(l : Exp, r : Exp) =>
                    new Div(l, r)
                case _ =>
                    illegalArgs("Div", "Exp, Exp", cs)
            }
        override def toString : String = s"Div($l,$r)"
    }

}
