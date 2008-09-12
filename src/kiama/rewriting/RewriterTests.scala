package kiama.rewriting

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalacheck._
import org.scalacheck.Prop._ 
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers 
import kiama.example.imperative.TestBase

/**
 * Run this to perform the tests.
 */
class RewriterTests extends TestCase with JUnit3Suite with Checkers
                    with Rewriter with TestBase {
                       
    import kiama.example.imperative.AST._
    
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
    
    def testSubtermMatching () {
        check ((t : Stmt) => issubterm (t) (t) == Some (t))
        check ((t : Exp) => issubterm (t) (t) == Some (t))
    
        val random = new scala.Random
    
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
    }
    
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
    
    def testFailure () {
        check ((t : Stmt) => failure (t) == None)
        check ((t : Exp) => failure (t) == None)
    }
    
    def testTermsAsStrategies () {
        check ((t : Stmt, u : Exp) => t (u) == Some (t))
        check ((t : Exp, u : Exp) => t (u) == Some (t))
        check ((t : Stmt, u : Stmt) => t (u) == Some (t))
        check ((t : Exp, u : Stmt) => t (u) == Some (t))
    }

}

