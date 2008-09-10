package kiama.example.imperative

/**
 * A simple imperative language abstract syntax.
 */
object AST {

    type Idn = String
    
    trait PrettyPrintable {
        def pretty (o : StringBuilder)
    }
    
    abstract class Exp extends Product with PrettyPrintable {
        val isconst : Boolean = false
        val value : Int = 0
        val vars : Set[Idn] = Set ()
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

    abstract class Stmt extends Product with PrettyPrintable {
        val vars : Set[Idn] = Set ()
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
     * Simple pretty-printer for statements.
     */
    def pretty (s : Stmt) : String = {
        val buffer = new StringBuilder
        s.pretty (buffer)
        buffer.toString
    }
    
    /**
     * Simple pretty-printer for expressions.
     */
    def pretty (e : Exp) : String = {
        val buffer = new StringBuilder
        e.pretty (buffer)
        buffer.toString
    }

}

/**
 * Parser to AST.
 */
trait Parser extends kiama.parsing.Packrat {

    import AST._
                
    val idn : Parser[String] =
        token (letter ~ (letterOrDigit*)) ^^ { case c ~ cs => c + cs.mkString }
    
    val variable : Parser[Var] =
        idn ^^ Var
    
    val number : Parser[Num] =
        token (digit+) ^^ (l => Num (l.mkString.toInt))
        
    val factor : Parser[Exp] =
        memo (number | variable | "(" ~> exp <~ ")")
    
    val term : Parser[Exp] =
        memo (term ~ ("*" ~> factor) ^^ { case l ~ r => Mul (l, r) } |
              term ~ ("/" ~> factor) ^^ { case l ~ r => Div (l, r) } |
              factor)
    
    val exp : Parser[Exp] =
        memo (exp ~ ("+" ~> term) ^^ { case l ~ r => Add (l, r) } |
              exp ~ ("-" ~> term) ^^ { case l ~ r => Sub (l, r) } |
              term)

    val sequence : Parser[Seqn] =
        "{" ~> (stmt+) <~ "}" ^^ Seqn
        
    val asgnStmt : Parser[Asgn] =
        idn ~ ("=" ~> exp) ^^ { case s ~ e => Asgn (s, e) }
    
    val whileStmt : Parser[While] =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ { case e ~ b => While (e, b) }

    val stmt : Parser[Stmt] =
        ";" ^^ (s => Null ()) | sequence | asgnStmt | whileStmt
       
}
        
/**
 * Basis for ScalaCheck tests using this language.  Support for generating
 * random AST instances plus convenient access to the parser and pretty-printer.
 */
trait TestBase extends Parser with PrettyPrinter {
    
    import AST._
    import org.scalacheck._

    val genNum = for (i <- Gen.choose (1,10)) yield Num (i)
    val genIdn = for (s <- Gen.identifier) yield (s)
    val genVar = for (v <- genIdn) yield Var (v)
    
    val genLeafExp = Gen.oneOf (genNum, genVar)

    def genAdd (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Add (l, r)
                    
    def genSub (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Sub (l, r)
                 
    def genMul (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Mul (l, r)

    def genDiv (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Div (l, r)
   
    def genInternalExp (sz : Int) =
        Gen.oneOf (genAdd (sz), genSub (sz), genMul (sz), genDiv (sz))
        
    def genExp (sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency ((1, genLeafExp), (3, genInternalExp (sz)))
                    
    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized (sz => genExp (sz)) }
    
    val genLeafStmt = Gen.value (Null ())
    
    def genSeqn (sz : Int) =
        for { len <- Gen.choose (1,sz)
              ss <- Gen.containerOfN[List,Stmt] (len, genStmt (sz / len)) }
            yield Seqn (ss)
            
    implicit def arbSeqn : Arbitrary[Seqn] =
        Arbitrary { Gen.sized (sz => genSeqn (sz)) }
    
    def genAsgn (sz : Int) =
        for { i <- genIdn; e <- genExp (sz-1) } yield Asgn (i, e)
    
    implicit def arbAsgn : Arbitrary[Asgn] =
        Arbitrary { Gen.sized (sz => genAsgn (sz)) }
                
    def genWhile (sz : Int) =
        for { e <- genExp (sz/3); b <- genStmt (sz - 1) } yield While (e, b)
        
    implicit def arbWhile : Arbitrary[While] =
        Arbitrary { Gen.sized (sz => genWhile (sz)) }

    def genInternalStmt (sz : Int) : Gen[Stmt] =
        Gen.frequency ((1, genSeqn (sz)), (5, genAsgn (sz)), (3, genWhile (sz)))
                
    def genStmt (sz : Int) =
        if (sz <= 0)
            genLeafStmt
        else
            Gen.frequency ((1, genLeafStmt), (9, genInternalStmt (sz)))
            
    implicit def arbStmt : Arbitrary[Stmt] =
        Arbitrary { Gen.sized (sz => genStmt (sz)) }
    
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
            case _ => println ("usage: Imperative [number]"); exit (1)
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
