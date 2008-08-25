package kiama.rewriting

import org.scalacheck._
import kiama.example.imperative.AST._

/**
 * Run this to perform the tests.
 */
object RewriterTests extends Application {
    
    trait TestBase extends kiama.example.imperative.TestBase with Rewriter
    
    object EvaluationTests extends Properties ("evaluation") with TestBase {
        val eval =
            rule {
                case Add (Num (i), Num (j)) => Num (i + j)
                case Sub (Num (i), Num (j)) => Num (i - j)
                case Var (_)                => Num (0)
            }
        specify ("eval", (t : Exp) => everywherebu (eval) (t) == Some (Num (t.value)))
    }
    
    object SubtermTests extends Properties ("subterm") with TestBase {
        specify ("root.stmt", (t : Stmt) => issubterm (t) (t) == Some (t))
        specify ("root.exp", (t : Exp) => issubterm (t) (t) == Some (t))
    
        private val random = new scala.util.Random
    
        /**
         * Pick a random Term child of t, returning t if there are no
         * children or there are children but none of them are Terms.
         */
        private def pickchild (t : Product) : Term = {
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
        private def pickdesc (t : Term) : Term = {
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
            
        specify ("desc.stmt", (t : Stmt) => issubterm (pickdesc (t)) (t) == Some (t))
        specify ("desc.exp", (t : Exp) => issubterm (pickdesc (t)) (t) == Some (t))
    }
    
    object NoChangeTests extends Properties ("nochange") with TestBase {
        specify ("id.stmt", (t : Stmt) => id (t) == Some (t))
        specify ("id.exp", (t : Exp) => id (t) == Some (t))
        
        val noopstmt = everywherebu (rule { case Asgn (s, e) => Asgn (s, e) })
        specify ("noopstmt.stmt", (t : Stmt) => noopstmt (t) == Some (t))
        specify ("noopstmt.exp", (t : Exp) => noopstmt (t) == Some (t))
        
        val noopexp = everywherebu (rule { case Num (i) => Num (i) })
        specify ("noopexp.stmt", (t : Stmt) => noopexp (t) == Some (t))
        specify ("noopexp.exp", (t : Exp) => noopexp (t) == Some (t))
    }
    
    object FailTests extends Properties ("fail") with TestBase {
        specify ("failure.stmt", (t : Stmt) => fail (t) == None)
        specify ("failure.exp", (t : Exp) => fail (t) == None)
    }
    
    object TermTests extends Properties ("term") with TestBase {
        specify ("const.stmt.exp", (t : Stmt, u : Exp) => t (u) == Some (t))
        specify ("const.exp.exp", (t : Exp, u : Exp) => t (u) == Some (t))
        specify ("const.stmt.stmt", (t : Stmt, u : Stmt) => t (u) == Some (t))
        specify ("const.exp.stmt", (t : Exp, u : Stmt) => t (u) == Some (t))
    }
    
    object AllTests extends Properties ("rewriting") {
        include (EvaluationTests)
        include (SubtermTests)
        include (NoChangeTests)
        include (FailTests)
        include (TermTests)
    }
    
    Test.checkProperties (AllTests)
}

