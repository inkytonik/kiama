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
    
        check ((t : Stmt) => issubterm (pickdesc (t), t) == Some (t))
        check ((t : Exp) => issubterm (pickdesc (t), t) == Some (t))
    }
    
    {
        val t = Add (Num (1), Num (2))
    
        test ("issubterm: selected subterms (fail)") {
            expect (None) (issubterm (Num (42), t))
        }
        
        test ("issubterm: selected subterms (succeed sub)") {
            expect (Some (t)) (issubterm (Num (1), t))
        }
        
        test ("issubterm: selected subterms (succeed self)") {
            expect (Some (t)) (issubterm (t, t))
        }
    
        test ("issubterm: selected proper subterms (fail)") {
            expect (None) (ispropersubterm (Num (42), t))
        }
        
        test ("issubterm: selected proper subterms (succeed sub)") {
            expect (Some (t)) (ispropersubterm (Num (1), t))
        }
        
        test ("issubterm: selected proper subterms (fail self)") {
            expect (None) (ispropersubterm (t, t))
        }
    
        test ("issuperterm: selected superterms (fail)") {
            expect (None) (issuperterm (t, Num (42)))
        }

        test ("issuperterm: selected superterms (succeed sub)") {
            expect (Some (t)) (issuperterm (t, Num (1)))
        }

        test ("issuperterm: selected superterms (succeed self)") {
            expect (Some (t)) (issuperterm (t, t))
        }
    
        test ("issuperterm: selected proper superterms (fail)") {
            expect (None) (ispropersuperterm (t, Num (42)))
        }
        
        test ("issuperterm: selected proper superterms (succeed sub)") {
            expect (Some (t)) (ispropersuperterm (t, Num (1)))
        }
        
        test ("issuperterm: selected proper superterms (fail self)") {
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

            test ("rewriting leaf types: increment doubles (all, bottomup)") {
                expect (Some (e)) ((allbu (double)) (e))
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
            
            test ("rewriting leaf types: reverse identifiers (all, bottomup)") {
                expect (Some (e)) ((allbu (rev)) (e))
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

            test ("rewriting leaf types: increment even doubles and reverse idn (all, bottomup)") {
                expect (Some (e)) ((allbu (evendoubleincrev)) (e))
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

    def travtest (desc : String, trav : (=> Strategy) => Strategy, rewl : Strategy,
                  term : Term, result : Option[Term]) =
        test (desc) {
            expect (result) (trav (rewl) (term))
        }

    {
        val l = List (Sub (Num (2), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        val r = List (Sub (Num (0), Var ("one")), Add (Num (0), Num (0)), Var ("two"))
        val s = List (Sub (Num (0), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        
        def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
            travtest ("rewrite list: doubles to zero in non-primitive list (" + desc + ")",
                      trav, rule { case _ : Double => 0 }, l, result)
        
        mktest ("all, topdown", alltd, Some (r))
        mktest ("all, bottomup", allbu, Some (l))
        mktest ("some, topdown", sometd, Some (r))
        mktest ("some, bottomup", somebu, Some (r))
        mktest ("one, topdown", oncetd, Some (s))
        mktest ("one, bottomup", oncebu, Some (s))
    }
    
    {
        val v = Set (1, 5, 8, 9)
        
        def mktest (desc : String, trav : (=> Strategy) => Strategy) =
            travtest ("rewrite set: no change (" + desc + ")",
                      trav, rule { case i : Int => i }, v, Some (v))
    
        mktest ("all, topdown", alltd)
        mktest ("all, bottomup", allbu)
        mktest ("some, topdown", sometd)
        mktest ("some, bottomup", somebu)
        mktest ("one, topdown", oncetd)
        mktest ("one, bottomup", oncebu)
    }

    {
        val r = Set (1, 5, 8, 9)
        val s = Set (2, 10, 16, 18)
        val t = Set (2, 5, 8, 9)

        def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
            travtest ("rewrite set: double value (" + desc + ")",
                      trav, rule { case i : Int => i * 2 }, r, result)

        mktest ("all, topdown", alltd, Some (s))
        mktest ("all, bottomup", allbu, Some (r))
        mktest ("some, topdown", sometd, Some (s))
        mktest ("some, bottomup", somebu, Some (s))
        mktest ("one, topdown", oncetd, Some (t))
        mktest ("one, bottomup", oncebu, Some (t))
    }

    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        
        def mktest (desc : String, trav : (=> Strategy) => Strategy) =
            travtest ("rewrite map: no change (" + desc + ")",
                      trav, rule { case s : String => s }, m, Some (m))

        mktest ("all, topdown", alltd)
        mktest ("all, bottomup", allbu)
        mktest ("some, topdown", sometd)
        mktest ("some, bottomup", somebu)
        mktest ("one, topdown", oncetd)
        mktest ("one, bottomup", oncebu)
    }
    
    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("eno" -> 1, "owt" -> 2, "eerht" -> 3)
        val s = Map ("eno" -> 1, "two" -> 2, "three" -> 3)

        def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
            travtest ("rewrite set: reverse keys (" + desc + ")",
                      trav, rule { case s : String => s.reverse }, m, result)
        
        mktest ("all, topdown", alltd, Some (r))
        mktest ("all, bottomup", allbu, Some (m))
        mktest ("some, topdown", sometd, Some (r))
        mktest ("some, bottomup", somebu, Some (r))
        mktest ("one, topdown", oncetd, Some (s))
        mktest ("one, bottomup", oncebu, Some (s))
    }
    
    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("one" -> 2, "two" -> 3, "three" -> 4)
        val s = Map ("one" -> 2, "two" -> 2, "three" -> 3)

        def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
            travtest ("rewrite set: increment values (" + desc + ")",
                      trav, rule { case i : Int => i + 1 }, m, result)
                
        mktest ("all, topdown", alltd, Some (r))
        mktest ("all, bottomup", allbu, Some (m))
        mktest ("some, topdown", sometd, Some (r))
        mktest ("some, bottomup", somebu, Some (r))
        mktest ("one, topdown", oncetd, Some (s))
        mktest ("one, bottomup", oncebu, Some (s))
    }
    
    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("eno" -> 2, "owt" -> 3, "eerht" -> 4)
        val s = Map ("eno" -> 1, "two" -> 2, "three" -> 3)

        def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
            travtest ("rewrite set: reverse keys and increment values (" + desc + ")",
                      trav, rule {
                                case s : String => s.reverse
                                case i : Int    => i + 1
                            }, m, result)
                
        mktest ("all, topdown", alltd, Some (r))
        mktest ("all, bottomup", allbu, Some (m))
        mktest ("some, topdown", sometd, Some (r))
        mktest ("some, bottomup", somebu, Some (r))
        mktest ("one, topdown", oncetd, Some (s))
        mktest ("one, bottomup", oncebu, Some (s))
    }
    
    {
        val m = Map (1 -> 2, 3 -> 4, 5 -> 6)
        val r = Map (2 -> 4, 4 -> 8, 6 -> 12)
        val s = Map (2 -> 4, 3 -> 4, 5 -> 6)
        
        def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
            travtest ("rewrite set: increment key and double value (" + desc + ")",
                      trav, rule { case (k : Int, v : Int) => (k + 1, v * 2) }, m, result)
        
        mktest ("all, topdown", alltd, Some (r))
        mktest ("all, bottomup", allbu, Some (m))
        mktest ("some, topdown", sometd, Some (r))
        mktest ("some, bottomup", somebu, Some (r))
        mktest ("one, topdown", oncetd, Some (s))
        mktest ("one, bottomup", oncebu, Some (s))
    }
    
    {
        // Maps from sets to their sizes, on init size is always zero
        val m1 = Map (Set (1, 3) -> 0, Set (2, 4, 6) -> 0)
        val m2 = Map (Set (12, 16) -> 0, Set (23) -> 0)
    
        // List of the maps
        val l = List (m1, m2)
    
        {
            val r = List (Map (Set (2, 4) -> 1, Set (3, 5, 7) -> 1),
                          Map (Set (13, 17) -> 1, Set (24) -> 1))
            val s = List (Map (Set (2, 3) -> 0, Set (2, 4, 6) -> 0),
                          Map (Set (12, 16) -> 0, Set (23) -> 0))

            def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
                travtest ("rewrite set: heterogeneous collection: inc integers (" + desc + ")",
                          trav, rule { case i : Int => i + 1 }, l, result)

            mktest ("all, topdown", alltd, Some (r))
            mktest ("all, bottomup", allbu, Some (l))
            mktest ("some, topdown", sometd, Some (r))
            mktest ("some, bottomup", somebu, Some (r))
            mktest ("one, topdown", oncetd, Some (s))
            mktest ("one, bottomup", oncebu, Some (s))
        }
    
        {
            val r = List (Map (Set (1, 3) -> 2, Set (2, 4, 6) -> 3),
                          Map (Set (12, 16) -> 2, Set (23) -> 1))
            val s = List (Map (Set (1, 3) -> 2, Set (2, 4, 6) -> 0),
                          Map (Set (12, 16) -> 0, Set (23) -> 0))

            def mktest (desc : String, trav : (=> Strategy) => Strategy, result : Option[Term]) =
                travtest ("rewrite set: heterogeneous collection: set to size (" + desc + ")",
                          trav, rule { case (s : Set[_], m) => (s, s.size) }, l, result)

            mktest ("all, topdown", alltd, Some (r))
            mktest ("all, bottomup", allbu, Some (l))
            mktest ("some, topdown", sometd, Some (r))
            mktest ("some, bottomup", somebu, Some (r))
            mktest ("one, topdown", oncetd, Some (s))
            mktest ("one, bottomup", oncebu, Some (s))
        }
    }
    
    {
        val incnum = rule { case Num (i) => Num (i + 1) }
        val inczerothchild = child (0, incnum)
        val incfirstchild = child (1, incnum)
        val incsecondchild = child (2, incnum)
        val incthirdchild = child (3, incnum)
        val incallsecondchild = alltd (incsecondchild)

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
                Asgn ("i", Num (10)),
                Asgn ("count", Num (0)),
                While (Var ("i"),
                    Seqn (List (
                        Asgn ("count", Add (Var ("count"), Num (1))),
                        Asgn ("i", Add (Num (1), Var ("i"))))))))
                        
        // { i = 0; count = 0; while (i) { count = bob + 1; i = 0 + i; } }
        val q = 
            Seqn (List (
                Asgn ("i", Num (0)),
                Asgn ("count", Num (0)),
                While (Var ("i"),
                    Seqn (List (
                        Asgn ("count", Add (Var ("bob"), Num (1))),
                        Asgn ("i", Add (Num (0), Var ("i"))))))))

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
