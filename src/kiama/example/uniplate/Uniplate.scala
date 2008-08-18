package kiama.example.uniplate

import Predef.{ int2Integer => _, _ }

import kiama.rewriting._
import org.scalacheck._

/**
 * Tests based on examples from the paper "Uniform boilerplate and list
 * processing" by Mitchell and Runciman, from Haskell workshop 2007.
 */

abstract class Exp extends Product {
    def value : Float
    def vars : Set[String] = Set ()
    def divsbyzero : Int = 0
    def depth : Int
    def intadds : Int = 0
}
case class Val (i : Int) extends Exp {
    def value = i
    def depth = 2
}
case class Var (s : String) extends Exp {
    def value = 3
    override def vars = Set (s)
    override def toString = "Var(\"" + s + "\")"
    def depth = 2
}
case class Neg (e : Exp) extends Exp {
    def value = - e.value
    override def vars = e.vars
    override def divsbyzero = e.divsbyzero
    def depth = 1 + e.depth
    override def intadds = e.intadds
}
abstract class Binary (l : Exp, r : Exp) extends Exp {
    override def vars = l.vars ++ r.vars
    override def divsbyzero = l.divsbyzero + r.divsbyzero
    def depth = 1 + (l.depth).max (r.depth)
    override def intadds = l.intadds + r.intadds
}
case class Add (l : Exp, r : Exp) extends Binary (l, r) {
    def value = l.value + r.value
    override def intadds =
        (l, r) match {
            case (Val (_), Val (_)) => 1
            case _                  => super.intadds
        }
}
case class Sub (l : Exp, r : Exp) extends Binary (l, r) {
    def value = l.value - r.value
}
case class Mul (l : Exp, r : Exp) extends Binary (l, r) {
    def value = l.value * r.value
}
case class Div (l : Exp, r : Exp) extends Binary (l, r) {
    def value = if (r.value == 0) 0 else l.value / r.value
    override def divsbyzero = 
        l.divsbyzero + (r match {
                           case Val (0) => 1
                           case _       => r.divsbyzero
                        })
}
        
trait TestBase extends Rewriter { 
    
    val genVal = for (i <- Gen.choose (0,10)) yield Val (i)
    val genIdn = for (i <- Gen.choose (1,5)) yield ("var" + i)
    val genVar = for (v <- genIdn) yield Var (v)
                
    val genLeafExp = Gen.oneOf (genVal, genVar)
        
    def genNeg (sz : Int) = 
        for { e <- sizedExp (sz/2) } yield Neg (e)
    
    def genAdd (sz : Int) =
        for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Add (l, r)
                    
    def genSub (sz : Int) =
        for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Sub (l, r)
    
    def genMul (sz : Int) =
        for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Mul (l, r)
                    
    def genDiv (sz : Int) =
        for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Div (l, r)
                    
    def genInternalExp (sz : Int) =
        Gen.oneOf (genNeg (sz), genAdd (sz), genSub (sz), genMul (sz), genDiv (sz))
        
    def sizedExp (sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency ((1, genLeafExp), (3, genInternalExp (sz)))
                    
    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized (sz => sizedExp (sz)) }

    // Simple expressions for use in tests where it doesn't make sense to
    // generate the test data automatically

    val numexp = Val (42)
    val varexp = Div (Mul (Var ("var1"), Var ("var2")), Var ("var1"))

}

/**
 * Tests that collect variable references.
 */
object VariablesTests extends Properties ("variables") with TestBase {
    // Direct style: local management of the collection
    def variables (e : Exp) : Set[String] = {
        var vars = Set[String]()
        everywheretd (query { case Var (s) => vars += s }) (e)
        vars
    }
    specify ("direct", (e : Exp) => variables (e) == e.vars)
        
    // Indirect: using the collects combinator to manage the set
    val variabless = collects { case Var (s) => s }
    specify ("indirect", (e : Exp) => variabless (e) == e.vars)

    // Simple check of set and list versions of collect
    val variablesl = collectl { case Var (s) => s }
    specify ("num.set", variabless (numexp) == Set ())
    specify ("num.list", variablesl (numexp) == List ())
    specify ("var.set", variabless (varexp) == Set ("var1", "var2"))
    specify ("var.list", variablesl (varexp) == List ("var1", "var2", "var1"))
}
    
/*
 * Arrange for divisions by zero to be more likely.
 */
trait DivsByZeroTestBase extends TestBase {
    override def genDiv (sz : Int) =
        Gen.frequency ((1, genDivByZero (sz)), (1, super.genDiv (sz)))
    def genDivByZero (sz : Int) =
        for { l <- sizedExp (sz/2) } yield Div (l, Val (0))
}

/**
 * Tests that search for divisions by literal zero.
 */
object DivsByZeroTests extends Properties ("divsbyzero") with DivsByZeroTestBase {
    def divsbyzero = count { case Div (_, Val (0)) => 1 }
    specify ("num", divsbyzero (numexp) == 0)
    specify ("var", divsbyzero (varexp) == 0)
    specify ("gen", (e : Exp) => divsbyzero (e) == e.divsbyzero)
}
    
/**
 * Tests of simplifying transformations.
 */
object SimplifyTests extends Properties ("simplify") with TestBase {
    def simplify : Exp => Exp =
        rewrite (everywheretd (rule {
            case Sub (x, y)           => simplify (Add (x, Neg (y)))
            case Add (x, y) if x == y => Mul (Val (2), x)
        }))
    specify ("num", simplify (numexp) == numexp)
    specify ("var", simplify (varexp) == varexp)

    val e = Sub (Add (Var ("a"), Var ("a")),
                 Add (Sub (Var ("b"), Val (1)), Sub (Var ("b"), Val (1))))
    val simpe = Add (Mul (Val (2), Var ("a")),
                     Neg (Mul (Val (2), Add (Var ("b"), Neg (Val (1))))))
    specify ("simp", simplify (e) == simpe)

    val f = Sub (Neg (Val (1)), Val (1))
    val simpf = Mul (Val (2), Neg (Val (1)))
    specify ("multi", simplify (f) == simpf)

    specify ("gen", (e : Exp) => simplify (e).value == e.value)
}
    
/*
 * Arrange for double negations to be more likely.
 */
trait DoubleNegTestBase extends TestBase {
    override def genNeg (sz : Int) = 
        Gen.frequency ((1, genDoubleNeg (sz)), (1, super.genNeg (sz)))
    def genDoubleNeg (sz : Int) =
        for { e <- super.genNeg (sz) } yield Neg (e)
}

/**
 * Tests that simplify double negations.
 */
object DoubleNegTests extends Properties ("doubleneg") with DoubleNegTestBase {
    def doubleneg : Exp => Exp =
        rewrite (everywherebu ( rule { case Neg (Neg (x)) => x }))
    specify ("num", doubleneg (numexp) == numexp)
    specify ("var", doubleneg (varexp) == varexp)
    specify ("gen", (e : Exp) => doubleneg (e).value == e.value)
}
    
/**
 * Tests of reciprocal division conversion to multiplication.
 */
object ReciprocalTests extends Properties ("reciprocal") with TestBase {
    def reciprocal : Exp => Exp =
        rewrite (everywherebu ( rule { case Div (n, m) => Mul (n, Div (Val (1), m)) }))
    def approximates (a : Float, b : Float) = (a - b).abs < 0.1
    specify ("gen", (e : Exp) => approximates (reciprocal (e).value, e.value))
}

/**
 * Tests that rename variables to be unique
 */
object UniqueVarsTests extends Properties ("uniquevars") with TestBase {
    def uniquevars : Exp => Exp =
        rewrite ({
            var count = 0
            everywheretd (rule { case Var (s) => count = count + 1; Var ("x" + count) })
        })
    specify ("num", uniquevars (numexp) == numexp)
    // Run this twice to make sure that count is not shared
    specify ("var1", uniquevars (varexp) == Div (Mul (Var ("x1"), Var ("x2")), Var ("x3")))
    specify ("var2", uniquevars (varexp) == Div (Mul (Var ("x1"), Var ("x2")), Var ("x3")))
    specify ("gen", (e : Exp) => uniquevars (e).value == e.value)
}

/**
 * Tests that calculate expression depth.
 */
object DepthTests extends Properties ("depth") with TestBase {    
    def maximum (l : Seq[Int]) : Int = l.drop (1).foldLeft (l.first)(_.max(_))
    def depth = para ((t : Any, cs : Seq[Int]) => 1 + maximum (List (0) ++ cs))
    specify ("num", depth (numexp) == 2)
    specify ("var", depth (varexp) == 4)
    specify ("gen", (e : Exp) => depth (e) == e.depth)
}

/**
 * Tests that rename variables.  Note that in the Uniplate paper and the Compos
 * paper before it, this is a multi-type example.  We do multi-type tests elsewhere.
 */
object RenameVarTests extends Properties ("renamevars") with TestBase {
    def rename : Exp => Exp =
        rewrite (everywheretd (rule { case Var (s) => Var ("_" + s) }))
    specify ("gen", (e : Exp) => rename (e).vars == e.vars.map ("_" + _))
}

/*
 * Arrange for integer additions to be more likely.
 */
trait OptimiseAddTestBase extends TestBase {
    override def genAdd (sz : Int) = 
        Gen.frequency ((1, genIntAdd (sz)), (1, super.genAdd (sz)))
    def genIntAdd (sz : Int) =
        for { l <- genVal; r <- genVal } yield Add (l, r)
}

/**
 * Optimisation of integer addition.
 */
object OptimiseAddTests extends Properties ("optimiseadd") with OptimiseAddTestBase {
    def optimiseadd : Exp => Exp =
        rewrite (everywherebu (rule {
            case Add (Val (n), Val (m)) => Val (n + m)
        }))
    specify ("gen", (e : Exp) => {
        val eopt = optimiseadd (e)
        (eopt.intadds == 0) && (eopt.value == e.value)
    })
}

/**
 * Collect all tests.
 */
object Uniplate extends Properties ("uniplate") {
    include (VariablesTests)
    include (DivsByZeroTests)
    include (SimplifyTests)
    include (DoubleNegTests)
    include (ReciprocalTests)
    include (UniqueVarsTests)
    include (DepthTests)
    include (RenameVarTests)
    include (OptimiseAddTests)
}

/**
 * Run these to perform the see test cases.  Argument should be number to see
 * (defaults to 5).
 */
trait Testcases {
    self : TestBase =>
    val name : String
    def main (args : Array[String]) = {
        var count = 0
        args.length match {
            case 0 => count = 5
            case 1 => count = args(0).toInt
            case _ => println ("usage: " + name + " [number]"); exit (1)
        }
        val genExp = Arbitrary.arbitrary[Exp]
        for (i <- 1 to count)
            println ("testcase " + i + ": " + genExp (Gen.defaultParams))
    }
}

object BasicTestCases extends Testcases with TestBase {
    val name = "Uniplate.BasicTestCases"
}

object DivsByZeroTestCases extends Testcases with DivsByZeroTestBase {
    val name = "Uniplate.DivsByZeroTestCases"
}

object DoubleNegTestCases extends Testcases with DoubleNegTestBase {
    val name = "Uniplate.DoubleNegTestCases"
}

object OptimiseAddTestCases extends Testcases with OptimiseAddTestBase {
    val name = "Uniplate.OptimiseAddTestCases"
}
