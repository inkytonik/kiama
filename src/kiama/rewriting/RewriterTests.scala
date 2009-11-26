/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2009 Anthony M Sloane, Macquarie University.
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

package org.kiama.rewriting

import org.kiama.example.imperative.Generator
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
 * Rewriting tests.
 */
class RewriterTests extends FunSuite with Checkers with Generator {

    import org.kiama.example.imperative.AST._
    import org.kiama.rewriting.Rewriter.{fail => rwfail, _}
    import org.scalacheck._
    import org.scalacheck.Prop._

    test ("basic arithmetic evaluation") {
        val eval =
            rule {
                case Add (Num (i), Num (j)) => Num (i + j)
                case Sub (Num (i), Num (j)) => Num (i - j)
                case Mul (Num (i), Num (j)) => Num (i * j)
                case Div (Num (i), Num (0)) => Num (0)  // Hack
                case Div (Num (i), Num (j)) => Num (i / j)
                case Var (_)                => Num (3)  // Hack
            }
        check ((t : Exp) => everywherebu (eval) (t) == Some (Num (t.value)))
        check ((t : Exp) => reduce (eval) (t) == Some (Num (t.value)))
    }

    test ("issubterm combinator") {
        check ((t : Stmt) => issubterm (t, t) == Some (t))
        check ((t : Exp) => issubterm (t, t) == Some (t))

        val random = new scala.util.Random

        /**
         * Pick a random Term child of t, returning t if there are no
         * children or there are children but none of them are Terms.
         */
        def pickchild (t : Product) : Term = {
            def isterm (c : Any) : Boolean = {
                c match {
                    case t : Term => true
                    case _        => false
                }
            }
            val children = for (i <- 0 until t.productArity) yield t.productElement (i)
            var childterms = children.filter (isterm)
            if (childterms.length == 0)
                // No term children, just use t itself
                t
            else {
                val termnum = random.nextInt (childterms.length)
                childterms (termnum).asInstanceOf[Term]
            }
        }

        /**
         * Pick a random descendant of t (including possibly t).
         */
        def pickdesc (t : Term) : Term = {
            t match {
                case p : Product =>
                    if (random.nextBoolean) {
                        pickchild (p)
                    } else {
                        val child = pickchild (p)
                        if (child == t)
                            t
                        else
                            pickdesc (child)
                    }
                case _ =>
                    t
            }
        }

        check ((t : Stmt) => issubterm (pickdesc (t), t) == Some (t))
        check ((t : Exp) => issubterm (pickdesc (t), t) == Some (t))

        val t = Add (Num (1), Num (2))

        expect (None) (issubterm (Num (42), t))
        expect (Some (t)) (issubterm (Num (1), t))
        expect (Some (t)) (issubterm (t, t))
        expect (None) (ispropersubterm (Num (42), t))
        expect (Some (t)) (ispropersubterm (Num (1), t))
        expect (None) (ispropersubterm (t, t))

        expect (None) (issuperterm (t, Num (42)))
        expect (Some (t)) (issuperterm (t, Num (1)))
        expect (Some (t)) (issuperterm (t, t))
        expect (None) (ispropersuperterm (t, Num (42)))
        expect (Some (t)) (ispropersuperterm (t, Num (1)))
        expect (None) (ispropersuperterm (t, t))
    }

    test ("strategies that have no effect") {
        check ((t : Stmt) => id (t) == Some (t))
        check ((t : Exp) => id (t) == Some (t))

        val noopstmt = everywherebu (rule { case Asgn (s, e) => Asgn (s, e) })
        check ((t : Stmt) => noopstmt (t) == Some (t))
        check ((t : Exp) => noopstmt (t) == Some (t))

        val noopexp = everywherebu (rule { case Num (i) => Num (i) })
        check ((t : Stmt) => noopexp (t) == Some (t))
        check ((t : Exp) => noopexp (t) == Some (t))
    }

    test ("strategies that fail immediately") {
        check ((t : Stmt) => rwfail (t) == None)
        check ((t : Exp) => rwfail (t) == None)
    }

    test ("where combinator") {
        check ((t : Exp) => where (rwfail) (t) == None)
        check ((t : Exp) => where (id) (t) == Some (t))
    }

    test ("leaf and innernode detection") {
        check ((t : Exp) =>
            isleaf (t) == (if (t.productArity == 0) Some (t) else None))
        check ((t : Exp) =>
            isinnernode (t) == (if (t.productArity == 0) None else Some (t)))
    }

    test ("terms as strategies") {
        check ((t : Stmt, u : Exp) => t (u) == Some (t))
        check ((t : Exp, u : Exp) => t (u) == Some (t))
        check ((t : Stmt, u : Stmt) => t (u) == Some (t))
        check ((t : Exp, u : Stmt) => t (u) == Some (t))

        check ((t : Stmt) => (term (t)) (t) == Some (t))
        check ((t : Exp) => (term (t)) (t) == Some (t))

        val t = Add (Num (1), Num (2))
        expect (None) (term (Num (1)) (t))
        expect (None) (term (Num (42)) (t))
    }

    test ("conditional choice operator") {
        // Test expressions
        val e1 = Mul (Num (2), Num (3))
        val e2 = Add (Num (2), Num (3))

        // Check identity and failure
        expect (Num (1)) (rewrite (id < (Num (1) : Strategy) + Num (2)) (e1))
        expect (Num (2)) (rewrite (rwfail < (Num (1) : Strategy) + Num (2)) (e1))

        // Condition used just for success or failure
        val ismulbytwo = rule { case t @ Mul (Num (2), _) => t }
        val multoadd = rule { case Mul (Num (2), x) => Add (x, x) }
        val error : Strategy = Num (99)
        val trans1 = ismulbytwo < multoadd + error
        expect (Add (Num (3), Num (3))) (rewrite (trans1) (e1))
        expect (Num (99)) (rewrite (trans1) (e2))

        // Condition that transforms subject
        val mulbytwotoadd = rule { case t @ Mul (Num (2), x) => Add (x, x) }
        val add = rule { case Add (_, _) => Num (42) }
        val trans2 = mulbytwotoadd < add + id
        expect (Num (42)) (rewrite (trans2) (e1))
        expect (Add (Num (2), Num (3))) (rewrite (trans2) (e2))
    }
    
    test ("strategies can return other strategies") {
        // Test expressions
        val e1 = Mul (Num (2), Num (5))
        val e2 = Add (Num (4), Num (5))
        
        // Single step passing
        val twotothree = rule { case Num (2) => Num (3) }
        val pass = rulefs { case Num (2) => twotothree }
        val passtd = everywheretd (pass)
        expect (Mul (Num (3), (Num (5)))) (rewrite (passtd) (e1))
        expect (Add (Num (4), (Num (5)))) (rewrite (passtd) (e2))
    }

}

