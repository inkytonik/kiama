package kiama.rewriting

import org.scalacheck._

/**
 * Run this to perform the tests.
 */
object RewriterTests extends Application {
    
    type Idn = String
    
    abstract class Exp extends Product {
        val isconst : Boolean = false
        val value : Int = 0
        val vars : Set[Idn] = Set ()
    }
    case class Num (i : Int) extends Exp {
        override val isconst = true
        override val value = i
    }
    case class Var (s : Idn) extends Exp {
        override def toString = "Var(\"" + s + "\")"
        override val vars = Set (s)
    }
    case class Add (l : Exp, r : Exp) extends Exp {
        override val isconst = l.isconst && r.isconst
        override val value = l.value + r.value
        override val vars = l.vars ++ r.vars
    }
    case class Sub (l : Exp, r : Exp) extends Exp {
        override val isconst = l.isconst && r.isconst
        override val value = l.value - r.value
        override val vars = l.vars ++ r.vars
    }

    abstract class Stmt extends Product {
        val vars : Set[Idn] = Set ()
    }
    case class Null extends Stmt
    case class Seqn (l : Int, ss : List[Stmt]) extends Stmt {
        override val vars = Set (ss flatMap (_ vars) : _*)
    }
    case class Asgn (s : Idn, e : Exp) extends Stmt {
        override val vars = Set (s)
    }
    case class While (e : Exp, b : Stmt) extends Stmt {
        override val vars = e.vars ++ b.vars
    }
        
    trait TestBase extends Rewriter { 
    
        val genNum = for (i <- Gen.choose (1,10)) yield Num (i)
        val genIdn = for (i <- Gen.choose (1,5)) yield ("var" + i)
        val genVar = for (v <- genIdn) yield Var (v)
                
        val genLeafExp = Gen.oneOf (genNum, genVar)
    
        def genAdd (sz : Int) =
            for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Add (l, r)
                    
        def genSub (sz : Int) =
            for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Sub (l, r)
                    
        def genInternalExp (sz : Int) = Gen.oneOf (genAdd (sz), genSub (sz))
        
        def sizedExp (sz : Int) : Gen[Exp] =
            if (sz <= 0)
                genLeafExp
            else
                Gen.frequency ((1, genLeafExp), (3, genInternalExp (sz)))
                    
        implicit def arbExp : Arbitrary[Exp] =
            Arbitrary { Gen.sized (sz => sizedExp (sz)) }
    
        val genLeafStmt = Gen.value (Null ())
    
        def genSeqn (sz : Int) =
            for { len <- Gen.choose (1,sz)
                  ss <- Gen.containerOfN[List,Stmt] (len, sizedStmt (sz / len)) }
                yield Seqn (sz, ss)
        
        def genAsgn (sz : Int) =
            for { i <- genIdn; e <- sizedExp (sz-1) } yield Asgn (i, e)
                    
        def genWhile (sz : Int) =
            for { e <- sizedExp (sz/3); b <- sizedStmt (2 * sz / 3) } yield While (e, b)
        
        def genInternalStmt (sz : Int) : Gen[Stmt] =
            Gen.frequency ((1, genSeqn (sz)), (5, genAsgn (sz)), (3, genWhile (sz)))
                
        def sizedStmt (sz : Int) =
            if (sz <= 0)
                genLeafStmt
            else
                Gen.frequency ((1, genLeafStmt), (9, genInternalStmt (sz)))
            
        implicit def arbStmt : Arbitrary[Stmt] =
            Arbitrary { Gen.sized (sz => sizedStmt (sz)) }
    
    }
    
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
    
        private val random = new Random
    
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

/**
 * Run this to perform the see test cases.  Argument should be number to see
 * (defaults to 5).
 */
object RewriterTestcases extends RewriterTests.TestBase {
    def main (args : Array[String]) = {
        var count = 0
        args.length match {
            case 0 => count = 5
            case 1 => count = args(0).toInt
            case _ => println ("usage: RewriterTestcases [number]"); exit (1)
        }
        val genStmt = Arbitrary.arbitrary[RewriterTests.Stmt]
        for (i <- 1 to count)
            println ("testcase " + i + ": " + genStmt (Gen.defaultParams))
    }
}
