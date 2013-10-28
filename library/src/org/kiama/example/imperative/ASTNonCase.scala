/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package example.imperative

/**
 * A version of the imperative expression abstract syntax built without
 * using case classes.  Used for testing on non-case class data structures.
 */
object ASTNonCase {

    import org.kiama.rewriting.Rewritable
    import scala.collection.immutable.Seq

    type Idn = String

    sealed abstract class Exp extends Rewritable

    class Num (val d : Double) extends Exp {
        def arity : Int = 1
        def deconstruct : Seq[Double] = Seq (d)
        def reconstruct (cs : Seq[Any]) : Exp =
            cs match {
                case Seq (d : Double) =>
                    new Num (d)
                case _ =>
                    illegalArgs ("Num", "Double", cs)
            }
        override def toString : String = s"Num($d)"
    }

    class Var (val s : Idn) extends Exp {
        def arity : Int = 1
        def deconstruct : Seq[Idn] = Seq (s)
        def reconstruct (cs : Seq[Any]) : Exp =
            cs match {
                case Seq (s : Idn) =>
                    new Var (s)
                case _ =>
                    illegalArgs ("Var", "Idn", cs)
            }
        override def toString : String = s"Var($s)"
    }

    class Neg (val e : Exp) extends Exp {
        def arity : Int = 1
        def deconstruct : Seq[Exp] = Seq (e)
        def reconstruct (cs : Seq[Any]) : Exp =
            cs match {
                case Seq (e : Exp) =>
                    new Neg (e)
                case _ =>
                    illegalArgs ("Neg", "Exp", cs)
            }
        override def toString : String = s"Neg($e)"
    }

    abstract class Binary (val l : Exp, val r : Exp) extends Exp {
        def arity : Int = 2
        def deconstruct : Seq[Exp] = Seq (l, r)
    }

    class Add (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Seq[Any]) : Exp =
            cs match {
                case Seq (l : Exp, r : Exp) =>
                    new Add (l, r)
                case _ =>
                    illegalArgs ("Add", "Exp, Exp", cs)
            }
        override def toString : String = s"Add($l,$r)"
    }

    class Sub (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Seq[Any]) : Exp =
            cs match {
                case Seq (l : Exp, r : Exp) =>
                    new Sub (l, r)
                case _ =>
                    illegalArgs ("Sub", "Exp, Exp", cs)
            }
        override def toString : String = s"Sub($l,$r)"
    }

    class Mul (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Seq[Any]) : Exp =
            cs match {
                case Seq (l : Exp, r : Exp) =>
                    new Mul (l, r)
                case _ =>
                    illegalArgs ("Mul", "Exp, Exp", cs)
            }
        override def toString : String = s"Mul($l,$r)"
    }

    class Div (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Seq[Any]) : Exp =
            cs match {
                case Seq (l : Exp, r : Exp) =>
                    new Div (l, r)
                case _ =>
                    illegalArgs ("Div", "Exp, Exp", cs)
            }
        override def toString : String = s"Div($l,$r)"
    }

}
