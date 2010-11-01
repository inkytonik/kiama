/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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
package rewriting

import org.kiama.example.imperative.Generator
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
 * Rewriting tests.
 */
@RunWith(classOf[JUnitRunner])
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

    test ("issubterm: a term is a subterm of itself") {
        check ((t : Stmt) => issubterm (t, t) == Some (t))
        check ((t : Exp) => issubterm (t, t) == Some (t))
    }

    test ("issubterm: random descendants are subterms") {
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
    }

    {
        val t = Add (Num (1), Num (2))

        test ("issubterm: selected subterms") {
            expect (None) (issubterm (Num (42), t))
            expect (Some (t)) (issubterm (Num (1), t))
            expect (Some (t)) (issubterm (t, t))
        }

        test ("issubterm: selected proper subterms") {
            expect (None) (ispropersubterm (Num (42), t))
            expect (Some (t)) (ispropersubterm (Num (1), t))
            expect (None) (ispropersubterm (t, t))
        }

        test ("issuperterm: selected superterms") {
            expect (None) (issuperterm (t, Num (42)))
            expect (Some (t)) (issuperterm (t, Num (1)))
            expect (Some (t)) (issuperterm (t, t))
        }

        test ("issuperterm: selected proper superterms") {
            expect (None) (ispropersuperterm (t, Num (42)))
            expect (Some (t)) (ispropersuperterm (t, Num (1)))
            expect (None) (ispropersuperterm (t, t))
        }
    }

    test ("strategies that have no effect: identity") {
        check ((t : Stmt) => id (t) == Some (t))
        check ((t : Exp) => id (t) == Some (t))
    }

    test ("strategies that have no effect: some terms to themselves") {
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

    test ("where: failure") {
        check ((t : Exp) => where (rwfail) (t) == None)
    }

    test ("where: identity") {
        check ((t : Exp) => where (id) (t) == Some (t))
    }

    test ("leaf detection") {
        check ((t : Exp) =>
            isleaf (t) == (if (t.productArity == 0) Some (t) else None))
    }

    test ("innernode detection") {
        check ((t : Exp) =>
            isinnernode (t) == (if (t.productArity == 0) None else Some (t)))
    }

    test ("terms as strategies") {
        check ((t : Stmt, u : Exp) => t (u) == Some (t))
        check ((t : Exp, u : Exp) => t (u) == Some (t))
        check ((t : Stmt, u : Stmt) => t (u) == Some (t))
        check ((t : Exp, u : Stmt) => t (u) == Some (t))
    }

    test ("term combinator") {
        check ((t : Stmt) => (term (t)) (t) == Some (t))
        check ((t : Exp) => (term (t)) (t) == Some (t))

        val t = Add (Num (1), Num (2))
        expect (None) (term (Num (1)) (t))
        expect (None) (term (Num (42)) (t))
    }

    {
        val e1 = Mul (Num (2), Num (3))
        val e2 = Add (Num (2), Num (3))

        test ("conditional choice operator: identity") {
            expect (Num (1)) (rewrite (id < (Num (1) : Strategy) + Num (2)) (e1))
        }

        test ("conditional choice operator: failure") {
            expect (Num (2)) (rewrite (rwfail < (Num (1) : Strategy) + Num (2)) (e1))
        }

        test ("conditional choice operator: condition for just success or failure") {
            val ismulbytwo = rule { case t @ Mul (Num (2), _) => t }
            val multoadd = rule { case Mul (Num (2), x) => Add (x, x) }
            val error : Strategy = Num (99)
            val trans1 = ismulbytwo < multoadd + error
            expect (Add (Num (3), Num (3))) (rewrite (trans1) (e1))
            expect (Num (99)) (rewrite (trans1) (e2))
        }

        test ("conditional choice operator: condition that transforms object") {
            val mulbytwotoadd = rule { case t @ Mul (Num (2), x) => Add (x, x) }
            val add = rule { case Add (_, _) => Num (42) }
            val trans2 = mulbytwotoadd < add + id
            expect (Num (42)) (rewrite (trans2) (e1))
            expect (Add (Num (2), Num (3))) (rewrite (trans2) (e2))
        }
    }

    test ("strategies can return another strategy") {
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

    {
        val e = Mul (Num (1), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))

        test ("rewriting leaf types: increment doubles") {
            val e = Mul (Num (1), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))
            val incint = everywheretd (rule { case d : Double => d + 1 })
            val r1 = Mul (Num (2), Add (Sub (Var ("hello"), Num (3)), Var ("harold")))
            expect (r1) (rewrite (incint) (e))
        }

        test ("rewriting leaf types: reverse identifiers") {
            val revidn = everywheretd (rule { case s : String => s.reverse })
            val r2 = Mul (Num (1), Add (Sub (Var ("olleh"), Num (2)), Var ("dlorah")))
            expect (r2) (rewrite (revidn) (e))
        }

        test ("rewriting leaf types: increment even doubles and reverse identifiers") {
            val incintrevidn =
                everywherebu (rule {
                    case i : Double if i < 2 => i + 1
                    case s : String => s.reverse
                })
            val r3 = Mul (Num (2), Add (Sub (Var ("olleh"), Num (2)), Var ("dlorah")))
            expect (r3) (rewrite (incintrevidn) (e))
        }
    }

    {
        val incall = everywheretd (rule { case i : Int => i + 1 })
        val incfirst = oncetd (rule { case i : Int => i + 1 })
        val incodd = sometd (rule { case i : Int if i % 2 == 1 => i + 1 })

        test ("rewriting lists: increment all numbers") {
            expect (List (2, 3, 4)) (rewrite (incall) (List (1, 2, 3)))
        }

        test ("rewriting lists: increment first number") {
            expect (List (2, 2, 3)) (rewrite (incfirst) (List (1, 2, 3)))
        }

        test ("rewriting lists: increment odd numbers") {
            expect (List (2, 2, 4)) (rewrite (incodd) (List (1, 2, 3)))
        }

        val l = List (List (1, 2), List (3), List (4, 5, 6))

        test ("rewriting lists: nested increment all numbers") {
            expect (List (List (2, 3), List (4), List (5, 6, 7))) (rewrite (incall) (l))
        }

        test ("rewriting lists: nested increment first number") {
            expect (List (List (2, 2), List (3), List (4, 5, 6))) (rewrite (incfirst) (l))
        }

        test ("rewriting lists: nested increment odd numbers") {
            expect (List (List (2, 2), List (4), List (4, 6, 6))) (rewrite (incodd) (l))
        }
    }

    test ("rewriting lists: doubles to zero in non-primitive list") {
        val ll = List (Sub (Num (2), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        val rr = List (Sub (Num (0), Var ("one")), Add (Num (0), Num (0)), Var ("two"))
        val numtozero = everywheretd (rule { case _ : Double => 0 })
        expect (rr) (rewrite (numtozero) (ll))
    }

}

