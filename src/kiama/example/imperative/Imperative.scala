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
        def pretty (o : StringBuilder)
    }
    case class Num (i : Int) extends Exp {
        override val isconst = true
        override val value = i
        def pretty (o : StringBuilder) = o.append (i)
    }
    case class Var (s : Idn) extends Exp {
        override def toString = "Var(\"" + s + "\")"
        override val vars = Set (s)
        def pretty (o : StringBuilder) = o.append (s)
    }
    case class Add (l : Exp, r : Exp) extends Exp {
        override val isconst = l.isconst && r.isconst
        override val value = l.value + r.value
        override val vars = l.vars ++ r.vars
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" + "); r.pretty (o); o.append (')')
        }
    }
    case class Sub (l : Exp, r : Exp) extends Exp {
        override val isconst = l.isconst && r.isconst
        override val value = l.value - r.value
        override val vars = l.vars ++ r.vars
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" - "); r.pretty (o); o.append (')')
        }
    }
    case class Mul (l : Exp, r : Exp) extends Exp {
        override val isconst = l.isconst && r.isconst
        override val value = l.value * r.value
        override val vars = l.vars ++ r.vars
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" * "); r.pretty (o); o.append (')')
        }
    }
    case class Div (l : Exp, r : Exp) extends Exp {
        override val isconst = l.isconst && r.isconst
        override val value = if (r.value == 0) 0 else l.value / r.value
        override val vars = l.vars ++ r.vars
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" / "); r.pretty (o); o.append (')')
        }
    }

    abstract class Stmt extends Product {
        val vars : Set[Idn] = Set ()
        def pretty (o : StringBuilder)
    }
    case class Null extends Stmt {
        def pretty (o : StringBuilder) = o.append (";\n")
    }
    case class Seqn (ss : List[Stmt]) extends Stmt {
        override val vars = Set (ss flatMap (_ vars) : _*)
        def pretty (o : StringBuilder) = {
            o.append ("{\n"); ss.foreach (_.pretty (o)); o.append ("}\n")
        }
    }
    case class Asgn (s : Idn, e : Exp) extends Stmt {
        override val vars = Set (s)
        def pretty (o : StringBuilder) = {
            o.append (s); o.append (" = "); e.pretty (o); o.append (";\n")
        }
    }
    case class While (e : Exp, b : Stmt) extends Stmt {
        override val vars = e.vars ++ b.vars
        def pretty (o : StringBuilder) = {
            o.append ("while ("); e.pretty (o); o.append (")\n"); 
            b.pretty (o);
        }
    }
    
}
    
/**
 * AST pretty-printing.
 */
trait PrettyPrinter {
    
    import AST._

    /**
     * Simple pretty-printer.
     */
    def pretty (s : Stmt) : String = {
        val buffer = new StringBuilder
        s.pretty (buffer)
        buffer.toString
    }

}

/**
 * Parser to AST.
 */
trait Parser extends kiama.parsing.Packrat {

    import AST._
    
    def parse : Parser[Stmt] =
        stmt

    def stmt : Parser[Stmt] =
        ";" ^^ (s => Null ()) | sequence | asgnStmt | whileStmt
        
    def sequence : Parser[Seqn] =
        "{" ~> (stmt+) <~ "}" ^^ Seqn
        
    def asgnStmt : Parser[Asgn] =
        idn ~ ("=" ~> exp) ^^ { case s ~ e => Asgn (s, e) }
    
    def whileStmt : Parser[While] =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ { case e ~ b => While (e, b) }

    def exp : Parser[Exp] =
        exp ~ ("+" ~> term) ^^ { case l ~ r => Add (l, r) } |
        exp ~ ("-" ~> term) ^^ { case l ~ r => Sub (l, r) } |
        term
    
    def term : Parser[Exp] =
        term ~ ("*" ~> factor) ^^ { case l ~ r => Mul (l, r) } |
        term ~ ("/" ~> factor) ^^ { case l ~ r => Div (l, r) } |
        factor

    def factor : Parser[Exp] =
        number | variable | "(" ~> exp <~ ")"

    def number : Parser[Num] =
        token (digit+) ^^ (l => Num (l.mkString.toInt))

    def variable : Parser[Var] =
        idn ^^ Var
        
    def idn : Parser[String] =
        token (letter ~ (letterOrDigit*)) ^^ { case c ~ cs => c + cs.mkString }

}
        
/**
 * Basis for ScalaCheck tests using this language.  Support for generating
 * random AST instances plus convenient access to the parser and pretty-printer.
 */
trait TestBase extends Parser with PrettyPrinter {
    
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
                 
    def genMul (sz : Int) =
        for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Mul (l, r)

    def genDiv (sz : Int) =
        for { l <- sizedExp (sz/2); r <- sizedExp (sz/2) } yield Div (l, r)
   
    def genInternalExp (sz : Int) =
        Gen.oneOf (genAdd (sz), genSub (sz), genMul (sz), genDiv (sz))
        
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
            yield Seqn (ss)
    
    def genAsgn (sz : Int) =
        for { i <- genIdn; e <- sizedExp (sz-1) } yield Asgn (i, e)
                
    def genWhile (sz : Int) =
        for { e <- sizedExp (sz/3); b <- sizedStmt (sz - 1) } yield While (e, b)
        
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

/**
 * Run this to perform the see test cases.  Argument should be number to see
 * (defaults to 5).
 */
object Imperative extends TestBase {

    import org.scalacheck._
    import AST._
    
    def main (args : Array[String]) = {
        var count = 0
        args.length match {
            case 0 => count = 5
            case 1 => count = args(0).toInt
            case _ => println ("usage: ImperativeTestcases [number]"); exit (1)
        }
        val genStmt = Arbitrary.arbitrary[Stmt]
        for (i <- 1 to count) {
            genStmt (Gen.defaultParams) match {
                case Some (s) => println ("testcase " + i + ": " + s + "\n" + pretty (s))
                case None     => println ("no testcases")
            }
        }
    }
}
