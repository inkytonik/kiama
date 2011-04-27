/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2011 Anthony M Sloane, Macquarie University.
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

    /**
     * Compare two optional terms.  Use reference equality for references
     * and value equality for non-reference values.
     */
    def same (v : Option[Term], optv : Option[Term]) : Boolean =
        (v, optv) match  {
            case (Some (v1 : AnyRef), Some (v2 : AnyRef)) => v1 eq v2
            case (Some (v1), Some (v2))                   => v1 == v2
            case (None, None)                             => true
            case _                                        => false
        }
        
    /**
     * Analogous to ScalaTest's expect but it uses same to compare
     * the two values instead of equality.
     */
    def expectsame (expected : Option[Term]) (actual : Option[Term]) {
        if (!same (expected, actual)) {
            fail ("Expected same object as " + expected + ", but got " + actual)
        }
    }

    /**
     * Analogous to ScalaTest's expect but it uses same to compare
     * the two values instead of equality.
     */
    def expectnotsame (expected : Option[Term]) (actual : Option[Term]) {
        if (same (expected, actual)) {
            fail ("Expected not same object as " + expected + ", but got " + actual)
        }
    }

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
        check ((t : Stmt) => same (Some (t), issubterm (t, t)))
        check ((t : Exp) => same (Some (t), issubterm (t, t)))
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
            val childterms = children.filter (isterm)
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
    
        check ((t : Stmt) => same (Some (t), issubterm (pickdesc (t), t)))
        check ((t : Exp) => same (Some (t), issubterm (pickdesc (t), t)))
    }
    
    {
        val t = Add (Num (1), Num (2))
    
        test ("issubterm: selected subterms (fail)") {
            expect (None) (issubterm (Num (42), t))
        }
        
        test ("issubterm: selected subterms (succeed sub)") {
            expectsame (Some (t)) (issubterm (Num (1), t))
        }
        
        test ("issubterm: selected subterms (succeed self)") {
            expectsame (Some (t)) (issubterm (t, t))
        }
    
        test ("issubterm: selected proper subterms (fail)") {
            expect (None) (ispropersubterm (Num (42), t))
        }
        
        test ("issubterm: selected proper subterms (succeed sub)") {
            expectsame (Some (t)) (ispropersubterm (Num (1), t))
        }
        
        test ("issubterm: selected proper subterms (fail self)") {
            expect (None) (ispropersubterm (t, t))
        }
    
        test ("issuperterm: selected superterms (fail)") {
            expect (None) (issuperterm (t, Num (42)))
        }
    
        test ("issuperterm: selected superterms (succeed sub)") {
            expectsame (Some (t)) (issuperterm (t, Num (1)))
        }
    
        test ("issuperterm: selected superterms (succeed self)") {
            expectsame (Some (t)) (issuperterm (t, t))
        }
    
        test ("issuperterm: selected proper superterms (fail)") {
            expect (None) (ispropersuperterm (t, Num (42)))
        }
        
        test ("issuperterm: selected proper superterms (succeed sub)") {
            expectsame (Some (t)) (ispropersuperterm (t, Num (1)))
        }
        
        test ("issuperterm: selected proper superterms (fail self)") {
            expect (None) (ispropersuperterm (t, t))
        }
    }
    
    test ("strategies that have no effect: identity") {
        check ((t : Stmt) => same (Some (t), id (t)))
        check ((t : Exp) => same (Some (t), id (t)))
    }
    
    test ("strategies that have no effect: some terms to themselves") {
        val noopstmt = everywherebu (rule { case Asgn (v, e) => Asgn (v, e) })
        check ((t : Stmt) => Some (t) == noopstmt (t))
        check ((t : Exp) => Some (t) == noopstmt (t))
    
        val noopexp = everywherebu (rule { case Num (i) => Num (i) })
        check ((t : Stmt) => Some (t) == noopexp (t))
        check ((t : Exp) => Some (t) == noopexp (t))
    }
    
    test ("strategies that fail immediately") {
        check ((t : Stmt) => rwfail (t) == None)
        check ((t : Exp) => rwfail (t) == None)
    }
    
    test ("where: failure") {
        check ((t : Exp) => where (rwfail) (t) == None)
    }
    
    test ("where: identity") {
        check ((t : Exp) => same (Some (t), where (id) (t)))
    }
    
    test ("leaf detection") {
        check ((t : Exp) =>
            same (if (t.productArity == 0) Some (t) else None, isleaf (t)))
    }
    
    test ("innernode detection") {
        check ((t : Exp) =>
            same (if (t.productArity == 0) None else Some (t), isinnernode (t)))
    }
    
    test ("terms as strategies") {
        check ((t : Stmt, u : Exp) => same (Some (t), t (u)))
        check ((t : Exp, u : Exp) => same (Some (t), t (u)))
        check ((t : Stmt, u : Stmt) => same (Some (t), t (u)))
        check ((t : Exp, u : Stmt) => same (Some (t), t (u)))
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
            expect (Some (Num (1))) ((id < (Num (1) : Strategy) + Num (2)) (e1))
        }
    
        test ("conditional choice operator: failure") {
            expect (Some (Num (2))) ((rwfail < (Num (1) : Strategy) + Num (2)) (e1))
        }
    
        test ("conditional choice operator: condition for just success or failure") {
            val ismulbytwo = rule { case t @ Mul (Num (2), _) => t }
            val multoadd = rule { case Mul (Num (2), x) => Add (x, x) }
            val error : Strategy = Num (99)
            val trans1 = ismulbytwo < multoadd + error
            expect (Some (Add (Num (3), Num (3)))) ((trans1) (e1))
            expect (Some (Num (99))) ((trans1) (e2))
        }
    
        test ("conditional choice operator: condition that transforms object") {
            val mulbytwotoadd = rule { case t @ Mul (Num (2), x) => Add (x, x) }
            val add = rule { case Add (_, _) => Num (42) }
            val trans2 = mulbytwotoadd < add + id
            expect (Some (Num (42))) ((trans2) (e1))
            expect (Some (Add (Num (2), Num (3)))) ((trans2) (e2))
        }
    }
    
    test ("strategies can return another strategy") {
        // Test expressions
        val e1 = Mul (Num (2), Num (5))
        val e2 = Add (Num (4), Num (5))
    
        // Single step passing
        val twotothree = rule { case Num (2) => Num (3) }
        val pass = rulefs { case Num (2) => twotothree }
        val passtd = everywhere (pass)
        expect (Some (Mul (Num (3), (Num (5))))) ((passtd) (e1))
        expect (Some (Add (Num (4), (Num (5))))) ((passtd) (e2))
    }
    
    {
        val e = Mul (Num (1), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))
        val ee = Mul (Num (1), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))
        
        test ("a bottomup traversal applying identity returns the same term") {
            expectsame (Some (e)) ((bottomup (id)) (e))
        }
    
        test ("a bottomup traversal applying identity doesn't returns term with same value") {
            expectnotsame (Some (ee)) ((bottomup (id)) (e))
        }
        
        test ("counting all terms using count") {
            val countall = count { case _ => 1 }
            expect (11) (countall (e))
        }
        
        test ("counting all terms using a para") {
            val countfold = 
                para[Int] {
                    case (t, cs) => 1 + cs.sum
                }
            expect (11) (countfold (e))
        }
            
        test ("counting all Num terms twice") {
            val countnum = count { case Num (_) => 2 }
            expect (4) (countnum (e))
        }
            
        test ("counting all Div terms") {
            val countdiv = count { case Div (_, _) => 1 }
            expect (0) (countdiv (e))
        }
            
        test ("counting all binary operator terms, with Muls twice") {
            val countbin = count {
                case Add (_, _) => 1
                case Sub (_, _) => 1
                case Mul (_, _) => 2
                case Div (_, _) => 1
            }
            expect (4) (countbin (e))
        }
        
        {
            val r = Mul (Num (2), Add (Sub (Var ("hello"), Num (3)), Var ("harold")))
            val s = Mul (Num (2), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))
        
            val double = rule { case d : Double => d + 1 }
        
            test ("rewriting leaf types: increment doubles (all, topdown)") {
                expect (Some (r)) ((alltd (double)) (e))
            }
        
            test ("rewriting leaf types: increment doubles (all, bottomup) same") {
                expectsame (Some (e)) ((allbu (double)) (e))
            }
        
            test ("rewriting leaf types: increment doubles (all, bottomup) not same") {
                expectnotsame (Some (ee)) ((allbu (double)) (e))
            }
        
            test ("rewriting leaf types: increment doubles (some, topdown)") {
                expect (Some (r)) ((sometd (double)) (e))
            }
        
            test ("rewriting leaf types: increment doubles (some, bottomup)") {
                expect (Some (r)) ((somebu (double)) (e))
            }
        
            test ("rewriting leaf types: increment doubles (one, topdown)") {
                expect (Some (s)) ((oncetd (double)) (e))
            }
        
            test ("rewriting leaf types: increment doubles (one, bottomup)") {
                expect (Some (s)) ((oncebu (double)) (e))
            }
        }
    
        {
            val r = Mul (Num (1), Add (Sub (Var ("olleh"), Num (2)), Var ("dlorah")))
            val s = Mul (Num (1), Add (Sub (Var ("olleh"), Num (2)), Var ("harold")))
            
            val rev = rule { case s : String => s.reverse }
            
            test ("rewriting leaf types: reverse identifiers (all, topdown)") {
                expect (Some (r)) ((alltd (rev)) (e))
            }
            
            test ("rewriting leaf types: reverse identifiers (all, bottomup) same") {
                expectsame (Some (e)) ((allbu (rev)) (e))
            }
            
            test ("rewriting leaf types: reverse identifiers (all, bottomup) not same") {
                expectnotsame (Some (ee)) ((allbu (rev)) (e))
            }
            
            test ("rewriting leaf types: reverse identifiers (some, topdown)") {
                expect (Some (r)) ((sometd (rev)) (e))
            }
            
            test ("rewriting leaf types: reverse identifiers (some, bottomup)") {
                expect (Some (r)) ((somebu (rev)) (e))
            }
            
            test ("rewriting leaf types: reverse identifiers (one, topdown)") {
                expect (Some (s)) ((oncetd (rev)) (e))
            }
            
            test ("rewriting leaf types: reverse identifiers (one, bottomup)") {
                expect (Some (s)) ((oncebu (rev)) (e))
            }
        }
            
        {
            val r = Mul (Num (2), Add (Sub (Var ("olleh"), Num (2)), Var ("dlorah")))
            val s = Mul (Num (2), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))
           
            val evendoubleincrev =
                rule {
                    case i : Double if i < 2 => i + 1
                    case s : String => s.reverse
                }
        
            test ("rewriting leaf types: increment even doubles and reverse idn (all, topdown)") {
                expect (Some (r)) ((alltd (evendoubleincrev)) (e))
            }
        
            test ("rewriting leaf types: increment even doubles and reverse idn (all, bottomup) same") {
                expectsame (Some (e)) ((allbu (evendoubleincrev)) (e))
            }
        
            test ("rewriting leaf types: increment even doubles and reverse idn (all, bottomup) not same") {
                expectnotsame (Some (ee)) ((allbu (evendoubleincrev)) (e))
            }
        
            test ("rewriting leaf types: increment even doubles and reverse idn (some, topdown)") {
                expect (Some (r)) ((sometd (evendoubleincrev)) (e))
            }
        
            test ("rewriting leaf types: increment even doubles and reverse idn (some, bottomup)") {
                expect (Some (r)) ((somebu (evendoubleincrev)) (e))
            }
        
            test ("rewriting leaf types: increment even doubles and reverse idn (one, topdown)") {
                expect (Some (s)) ((oncetd (evendoubleincrev)) (e))
            }
        
            test ("rewriting leaf types: increment even doubles and reverse idn (one, bottomup)") {
                expect (Some (s)) ((oncebu (evendoubleincrev)) (e))
            }
        }
    }
    
    test ("rewrite to increment an integer") {
        val inc = rule { case i : Int => i + 1 }
        expect (Some (4)) ((inc) (3))
    }
    
    test ("rewrite failing to increment an integer with a double increment") {
        val inc = rule { case d : Double => d + 1 }
        expect (None) ((inc) (3))
    }
    
    {
        val incall = alltd (rule { case i : Int => i + 1 })
        val incfirst = oncetd (rule { case i : Int => i + 1 })
        val incodd = sometd (rule { case i : Int if i % 2 == 1 => i + 1 })
    
        test ("rewrite list: increment all numbers (non-empty)") {
            expect (Some (List (2, 3, 4))) ((incall) (List (1, 2, 3)))
        }
        
        test ("rewrite list: increment all numbers (empty)") {
            expect (Some (Nil)) ((incall) (Nil))
        }
        
        test ("rewrite list: increment first number (non-empty)") {
            expect (Some (List (2, 2, 3))) ((incfirst) (List (1, 2, 3)))
        }
        
        test ("rewrite list: increment first number (empty)") {
            expect (None) ((incfirst) (Nil))
        }
        
        test ("rewrite list: increment odd numbers (succeed") {
            expect (Some (List (2, 2, 4))) ((incodd) (List (1, 2, 3)))
        }
        
        test ("rewrite list: increment odd numbers (fail)") {
            expect (None) ((incodd) (List (2, 4, 6)))
        }
        
        val l = List (List (1, 2), List (3), List (4, 5, 6))
    
        test ("rewrite list: nested increment all numbers") {
            expect (Some (List (List (2, 3), List (4), List (5, 6, 7)))) ((incall) (l))
        }
        
        test ("rewrite list: nested increment first number") {
            expect (Some (List (List (2, 2), List (3), List (4, 5, 6)))) ((incfirst) (l))
        }
        
        test ("rewrite list: nested increment odd numbers (succeed)") {
            expect (Some (List (List (2, 2), List (4), List (4, 6, 6)))) ((incodd) (l))
        }
        
        test ("rewrite list: nested increment odd numbers (fail)") {
            expect (None) ((incodd) (List (List (2, 2), List (4), List (4, 6, 6))))
        }
    }
    
    /**
     * The kind of comparison that is expected to be true for a test.  Equal
     * means use ==.  Same means the result must be the same reference or, if
     * the values are not references, use ==.  NotSame is the opposite of Same.
     */
    abstract class Expecting
    case object Equal extends Expecting
    case object Same extends Expecting
    case object NotSame extends Expecting
    
    def travtest (basemsg : String, testmsg : String, trav : (=> Strategy) => Strategy,
                  rewl : Strategy, term : Term, result : Option[Term],
                  expecting : Expecting = Equal) = {
        val msg = basemsg + " (" + testmsg + ") " + expecting
        test (msg) {
            expecting match {
                case Equal   => expect (result) (trav (rewl) (term))
                case Same    => expectsame (result) (trav (rewl) (term))
                case NotSame => expectnotsame (result) (trav (rewl) (term))
            }
        }
    }
    
    {
        val l = List (Sub (Num (2), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        val ll = List (Sub (Num (2), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        val r = List (Sub (Num (0), Var ("one")), Add (Num (0), Num (0)), Var ("two"))
        val s = List (Sub (Num (0), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        
        val strat = rule { case _ : Double => 0 }
        val basemsg = "rewrite list: doubles to zero in non-primitive list"
    
        travtest (basemsg, "all, topdown", alltd, strat, l, Some (r))
        travtest (basemsg, "all, bottomup", allbu, strat, l, Some (l), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, l, Some (ll), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, l, Some (r))
        travtest (basemsg, "some, bottomup", somebu, strat, l, Some (r))
        travtest (basemsg, "one, topdown", oncetd, strat, l, Some (s))
        travtest (basemsg, "one, bottomup", oncebu, strat, l, Some (s))
    }
    
    {
        val v = Set (1, 5, 8, 9)
        val vv = Set (1, 5, 8, 9)
        
        val strat = rule { case i : Int => i }
        val basemsg = "rewrite set: no change"
    
        travtest (basemsg, "all, topdown", alltd, strat, v, Some (v), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, v, Some (v), Same)
        travtest (basemsg, "some, topdown", sometd, strat, v, Some (v), Same)
        travtest (basemsg, "some, bottomup", somebu, strat, v, Some (v), Same)
        travtest (basemsg, "one, topdown", oncetd, strat, v, Some (v), Same)
        travtest (basemsg, "one, bottomup", oncebu, strat, v, Some (v), Same)
    
        travtest (basemsg, "all, topdown", alltd, strat, v, Some (vv), NotSame)
        travtest (basemsg, "all, bottomup", allbu, strat, v, Some (vv), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, v, Some (vv), NotSame)
        travtest (basemsg, "some, bottomup", somebu, strat, v, Some (vv), NotSame)
        travtest (basemsg, "one, topdown", oncetd, strat, v, Some (vv), NotSame)
        travtest (basemsg, "one, bottomup", oncebu, strat, v, Some (vv), NotSame)
    }
    
    {
        val r = Set (1, 5, 8, 9)
        val rr = Set (1, 5, 8, 9)
        val s = Set (2, 10, 16, 18)
        val t = Set (2, 5, 8, 9)
    
        val strat = rule { case i : Int => i * 2 }
        val basemsg = "rewrite set: double value"
    
        travtest (basemsg, "all, topdown", alltd, strat, r, Some (s))
        travtest (basemsg, "all, bottomup", allbu, strat, r, Some (r), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, r, Some (rr), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, r, Some (s))
        travtest (basemsg, "some, bottomup", somebu, strat, r, Some (s))
        travtest (basemsg, "one, topdown", oncetd, strat, r, Some (t))
        travtest (basemsg, "one, bottomup", oncebu, strat, r, Some (t))
    }
    
    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        
        val strat = rule { case s : String => s }
        val basemsg = "rewrite map: no change"
    
        travtest (basemsg, "all, topdown", alltd, strat, m, Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (m), Same)
        travtest (basemsg, "some, topdown", sometd, strat, m, Some (m), Same)
        travtest (basemsg, "some, bottomup", somebu, strat, m, Some (m), Same)
        travtest (basemsg, "one, topdown", oncetd, strat, m, Some (m), Same)
        travtest (basemsg, "one, bottomup", oncebu, strat, m, Some (m), Same)
    
        travtest (basemsg, "all, topdown", alltd, strat, m, Some (mm), NotSame)
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, m, Some (mm), NotSame)
        travtest (basemsg,"some, bottomup", somebu, strat, m, Some (mm), NotSame)
        travtest (basemsg, "one, topdown", oncetd, strat, m, Some (mm), NotSame)
        travtest (basemsg, "one, bottomup", oncebu, strat, m, Some (mm), NotSame)
    }
    
    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("eno" -> 1, "owt" -> 2, "eerht" -> 3)
        val s = Map ("eno" -> 1, "two" -> 2, "three" -> 3)
    
        val strat = rule { case s : String => s.reverse }
        val basemsg = "rewrite set: reverse keys"
        
        travtest (basemsg, "all, topdown", alltd, strat, m, Some (r))
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, m, Some (r))
        travtest (basemsg, "some, bottomup", somebu, strat, m, Some (r))
        travtest (basemsg, "one, topdown", oncetd, strat, m, Some (s))
        travtest (basemsg, "one, bottomup", oncebu, strat, m, Some (s))
    }
    
    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("one" -> 2, "two" -> 3, "three" -> 4)
        val s = Map ("one" -> 2, "two" -> 2, "three" -> 3)
    
        val strat = rule { case i : Int => i + 1 }
        val basemsg = "rewrite set: increment values"
        
        travtest (basemsg, "all, topdown", alltd, strat, m, Some (r))
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, m, Some (r))
        travtest (basemsg, "some, bottomup", somebu, strat, m, Some (r))
        travtest (basemsg, "one, topdown", oncetd, strat, m, Some (s))
        travtest (basemsg, "one, bottomup", oncebu, strat, m, Some (s))
    }
    
    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("eno" -> 2, "owt" -> 3, "eerht" -> 4)
        val s = Map ("eno" -> 1, "two" -> 2, "three" -> 3)
        
        val basemsg = "rewrite set: reverse keys and increment values"
        val strat = rule {
                        case s : String => s.reverse
                        case i : Int    => i + 1
                    }
        
        travtest (basemsg, "all, topdown", alltd, strat, m, Some (r))
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, m, Some (r))
        travtest (basemsg, "some, bottomup", somebu, strat, m, Some (r))
        travtest (basemsg, "one, topdown", oncetd, strat, m, Some (s))
        travtest (basemsg, "one, bottomup", oncebu, strat, m, Some (s))
    }
    
    {
        val m = Map (1 -> 2, 3 -> 4, 5 -> 6)
        val mm = Map (1 -> 2, 3 -> 4, 5 -> 6)
        val r = Map (2 -> 4, 4 -> 8, 6 -> 12)
        val s = Map (2 -> 4, 3 -> 4, 5 -> 6)
        
        val basemsg = "rewrite set: increment key and double value"
        val strat = rule { case (k : Int, v : Int) => (k + 1, v * 2) }
        
        travtest (basemsg, "all, topdown", alltd, strat, m, Some (r))
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu, strat, m, Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd, strat, m, Some (r))
        travtest (basemsg, "some, bottomup", somebu, strat, m, Some (r))
        travtest (basemsg, "one, topdown", oncetd, strat, m, Some (s))
        travtest (basemsg, "one, bottomup", oncebu, strat, m, Some (s))
    }
    
    {
        // Maps from sets to their sizes, on init size is always zero
        val m1 = Map (Set (1, 3) -> 0, Set (2, 4, 6) -> 0)
        val m2 = Map (Set (12, 16) -> 0, Set (23) -> 0)
    
        // List of the maps
        val l = List (m1, m2)
        val ll = List (Map (Set (1, 3) -> 0, Set (2, 4, 6) -> 0),
                       Map (Set (12, 16) -> 0, Set (23) -> 0))
    
        {
            val r = List (Map (Set (2, 4) -> 1, Set (3, 5, 7) -> 1),
                          Map (Set (13, 17) -> 1, Set (24) -> 1))
            val s = List (Map (Set (2, 3) -> 0, Set (2, 4, 6) -> 0),
                          Map (Set (12, 16) -> 0, Set (23) -> 0))
            
            val basemsg = "rewrite set: heterogeneous collection: inc integers"
            val strat = rule { case i : Int => i + 1 }
            
            travtest (basemsg, "all, topdown", alltd, strat, l, Some (r))
            travtest (basemsg, "all, bottomup", allbu, strat, l, Some (l), Same)
            travtest (basemsg, "all, bottomup", allbu, strat, l, Some (ll), NotSame)
            travtest (basemsg, "some, topdown", sometd, strat, l, Some (r))
            travtest (basemsg, "some, bottomup", somebu, strat, l, Some (r))
            travtest (basemsg, "one, topdown", oncetd, strat, l, Some (s))
            travtest (basemsg, "one, bottomup", oncebu, strat, l, Some (s))
        }
    
        {
            val r = List (Map (Set (1, 3) -> 2, Set (2, 4, 6) -> 3),
                          Map (Set (12, 16) -> 2, Set (23) -> 1))
            val s = List (Map (Set (1, 3) -> 2, Set (2, 4, 6) -> 0),
                          Map (Set (12, 16) -> 0, Set (23) -> 0))
            
            val basemsg = "rewrite set: heterogeneous collection: set to size"
            val strat = rule { case (s : Set[_], m) => (s, s.size) }
            
            travtest (basemsg, "all, topdown", alltd, strat, l, Some (r))
            travtest (basemsg, "all, bottomup", allbu, strat, l, Some (l), Same)
            travtest (basemsg, "all, bottomup", allbu, strat, l, Some (ll), NotSame)
            travtest (basemsg, "some, topdown", sometd, strat, l, Some (r))
            travtest (basemsg, "some, bottomup", somebu, strat, l, Some (r))
            travtest (basemsg, "one, topdown", oncetd, strat, l, Some (s))
            travtest (basemsg, "one, bottomup", oncebu, strat, l, Some (s))
        }
    }
    
    {
        val l = Add (Num (1), Num (2))
        val r = Add (Num (3), Num (4))
        val t = Sub (l, r)

        val incnum = rule { case Num (i) => Num (i + 1) }
        val inczerothchild = child (0, incnum)
        val incfirstchild = child (1, incnum)
        val incsecondchild = child (2, incnum)
        val incthirdchild = child (3, incnum)
        val incallsecondchild = alltd (incsecondchild)
        val addtomul = rule { case Add (l, r) => Mul (l, r) }
        
        test ("rewrite by child index: index too low is failure") {
            expect (None) ((child (-3, addtomul)) (t))
        }
            
        test ("rewrite by child index: index too high is failure") {
            expect (None) ((child (3, addtomul)) (t))
        }
            
        test ("rewrite by child index: only rewritten child is replaced") {
            val u = Sub (Add (Num (1), Num (2)), Mul (Num (3), Num (4)))
            val v = (child (2, addtomul)) (t)
            expect (Some (u)) (v)
            val Some (w) = v
            expectsame (Some (l)) (Some (w.asInstanceOf[Sub].l))
        }
        
        test ("rewrite by child index: inc zeroth child (fail)") {
            expect (None) (inczerothchild (Add (Num (2), Num (3))))
        }
    
        test ("rewrite by child index: inc first child (fail)") {
            expect (None) (incfirstchild (Num (2)))
        }
    
        test ("rewrite by child index: inc first child (succeed, one child, one level)") {
            expect (Some (Neg (Num (3)))) (incfirstchild (Neg (Num (2))))
        }
    
        test ("rewrite by child index: inc first child (succeed, two children, one level)") {
            expect (Some (Add (Num (3), Num (3)))) (incfirstchild (Add (Num (2), Num (3))))
        }
    
        test ("rewrite by child index: inc second child (fail)") {
            expect (None) (incsecondchild (Num (2)))
        }
    
        test ("rewrite by child index: inc second child (succeed, one level)") {
            expect (Some (Add (Num (2), Num (4)))) (incsecondchild (Add (Num (2), Num (3))))
        }
    
        test ("rewrite by child index: inc third child (fail, one level)") {
            expect (None) (incthirdchild (Add (Num (2), Num (3))))
        }
    
        test ("rewrite by child index: inc second child (succeed, multi-level)") {
            expect (Some (Sub (Add (Num (2), Num (4)), Mul (Num (4), Num (6))))) (
                incallsecondchild (Sub (Add (Num (2), Num (3)), Mul (Num (4), Num (5))))
            )
        }
    }
    
    {
        // The type used here should be a Seq that is not implemented using case classes
        // (or other Products)
        import scala.collection.mutable.LinkedList
        
        val incint = rule { case i : Int => i + 1 }
        val inczerothchild = child (0, incint)
        val incfirstchild = child (1, incint)
        val incsecondchild = child (2, incint)
        val incallsecondchild = alltd (incsecondchild)
    
        val l1 = LinkedList ()
        val l2 = LinkedList (1)
        val l3 = LinkedList (1, 2, 3, 4)
        
        test ("rewrite linkedlist by child index: inc zeroth child (fail, empty)") {
            expect (None) (inczerothchild (l1))
        }
    
        test ("rewrite linkedlist by child index: inc first child (fail, empty)") {
            expect (None) (incfirstchild (l1))
        }
    
        test ("rewrite linkedlist by child index: inc first child (succeed, singleton)") {
            expect (Some (LinkedList (2))) (incfirstchild (l2))
        }        
                
        test ("rewrite linkedlist by child index: inc second child (fail, singleton)") {
            expect (None) (incsecondchild (l2))
        }        
        
        test ("rewrite linkedlist by child index: inc zeroth child (fail, multiple)") {
            expect (None) (inczerothchild (l3))
        }        
        
        test ("rewrite linkedlist by child index: inc first child (succeed, multiple)") {
            expect (Some (LinkedList (2, 2, 3, 4))) (incfirstchild (l3))
        }        
                
        test ("rewrite linkedlist by child index: inc second child (succeed, one level)") {
            expect (Some (LinkedList (1, 3, 3, 4))) (incsecondchild (l3))
        }        
    
        test ("rewrite linkedlist by child index: inc second child (succeed, multi-level)") {
            expect (Some (LinkedList (LinkedList (1), LinkedList (3, 5, 5), LinkedList (6, 8)))) (
                incallsecondchild (LinkedList (LinkedList (1), LinkedList (3, 4, 5), LinkedList (6, 7)))
            )
        }
    }
    
    {
        // { i = 10; count = 0; while (i) { count = count + 1; i = 1 + i; } }
        val p = 
            Seqn (List (
                Asgn (Var ("i"), Num (10)),
                Asgn (Var ("count"), Num (0)),
                While (Var ("i"),
                    Seqn (List (
                        Asgn (Var ("count"), Add (Var ("count"), Num (1))),
                        Asgn (Var ("i"), Add (Num (1), Var ("i"))))))))
                        
        // { i = 0; count = 0; while (i) { count = bob + 1; i = 0 + i; } }
        val q = 
            Seqn (List (
                Asgn (Var ("i"), Num (0)),
                Asgn (Var ("count"), Num (0)),
                While (Var ("i"),
                    Seqn (List (
                        Asgn (Var ("count"), Add (Var ("bob"), Num (1))),
                        Asgn (Var ("i"), Add (Num (0), Var ("i"))))))))
    
        val incint = rule { case i : Int => i + 1 }
        val clearlist = rule { case _ => Nil }
        val zeronumsbreakadds =
            alltd (Num (rule { case _ => 0}) +
                   Add (rule { case Var (_) => Var ("bob")}, id))
    
        test ("rewrite by congruence: top-level wrong congruence") {
            expect (None) (Num (incint) (p))
        }
        
        test ("rewrite by congruence: top-level correct congruence") {
            expect (Some (Seqn (Nil))) (Seqn (clearlist) (p))
        }
        
        test ("rewrite by congruence: multi-level") {
            expect (Some (q)) (zeronumsbreakadds (p))
        }
    }

}
