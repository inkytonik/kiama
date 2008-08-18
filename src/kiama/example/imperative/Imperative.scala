package kiama.example.imperative

/**
 * A simple imperative language abstract syntax.
 */
object AST {

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

}
        
/**
 * Basis for ScalaCheck tests using this language.  Support for generating
 * random AST instances.
 */
trait TestBase {
    
    import AST._
    import org.scalacheck._

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
