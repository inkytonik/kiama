/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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
                                
package kiama.rewriting

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalacheck._
import org.scalacheck.Prop._ 
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers 
import kiama.example.imperative.TestBase

/**
 * Rewriting tests.
 */
class RewriterTests extends TestCase with JUnit3Suite with Checkers
                    with Rewriter with TestBase {
                       
    import kiama.example.imperative.AST._
    
    /**
     * Test arithmetic evaluation with variable references and division by
     * zero worked around.
     */
    def testEvaluation () {
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
    }
    
    /**
     * Test the issubterm combinator.
     */
    def testSubtermMatching () {
        check ((t : Stmt) => issubterm (t) (t) == Some (t))
        check ((t : Exp) => issubterm (t) (t) == Some (t))
    
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
            
        check ((t : Stmt) => issubterm (pickdesc (t)) (t) == Some (t))
        check ((t : Exp) => issubterm (pickdesc (t)) (t) == Some (t))

        assertEquals (None, issubterm (Num (42)) (Add (Num (1), Num (2))))
    }
    
    /**
     * Test strategies that should have no effect on the subject term.
     */
    def testNoChange () {
        check ((t : Stmt) => id (t) == Some (t))
        check ((t : Exp) => id (t) == Some (t))
        
        val noopstmt = everywherebu (rule { case Asgn (s, e) => Asgn (s, e) })
        check ((t : Stmt) => noopstmt (t) == Some (t))
        check ((t : Exp) => noopstmt (t) == Some (t))
        
        val noopexp = everywherebu (rule { case Num (i) => Num (i) })
        check ((t : Stmt) => noopexp (t) == Some (t))
        check ((t : Exp) => noopexp (t) == Some (t))
    }
    
    /**
     * Test strategies that fail immediately.
     */
    def testFailure () {
        check ((t : Stmt) => failure (t) == None)
        check ((t : Exp) => failure (t) == None)
    }
    
    /**
     * Test strategies defined from a specific term.
     */
    def testTermsAsStrategies () {
        check ((t : Stmt, u : Exp) => t (u) == Some (t))
        check ((t : Exp, u : Exp) => t (u) == Some (t))
        check ((t : Stmt, u : Stmt) => t (u) == Some (t))
        check ((t : Exp, u : Stmt) => t (u) == Some (t))
    }
    
    /**
     * Simple tests of conditional choice strategy combinator.
     */
    def testConditional () {
        // Test expressions
        val e1 = Mul (Num (2), Num (3))
        val e2 = Add (Num (2), Num (3))
        
        // Check identity and failure
        assertEquals (Num (1), rewrite (id < (Num (1) : Strategy) + Num (2)) (e1))
        assertEquals (Num (2), rewrite (failure < (Num (1) : Strategy) + Num (2)) (e1))
        
        // Condition used just for success or failure
        val ismulbytwo = rule { case t @ Mul (Num (2), _) => t }
        val multoadd = rule { case Mul (Num (2), x) => Add (x, x) }
        val error : Strategy = Num (99)
        val trans1 = ismulbytwo < multoadd + error
        assertEquals (Add (Num (3), Num (3)), rewrite (trans1) (e1))
        assertEquals (Num (99), rewrite (trans1) (e2))

        // Condition that transforms subject
        val mulbytwotoadd = rule { case t @ Mul (Num (2), x) => Add (x, x) }
        val add = rule { case Add (_, _) => Num (42) }
        val trans2 = mulbytwotoadd < add + id
        assertEquals (Num (42), rewrite (trans2) (e1))
        assertEquals (Add (Num (2), Num (3)), rewrite (trans2) (e2))
    }

}

