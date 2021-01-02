/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package rewriting

import org.bitbucket.inkytonik.kiama.example.imperative.Generator
import org.bitbucket.inkytonik.kiama.util.KiamaTests

/**
 * Support for rewriting tests.
 */
object RewriterTests {

    import java.lang.IndexOutOfBoundsException

    /**
     * A user-defined singleton case object.
     */
    case object SingleCaseObject

    /**
     * A user-defined singleton object that is a product. Essentially
     * this is similar to `Nil`.
     */
    object SingleObject extends Product {
        def canEqual(that : Any) = true
        def productArity = 0
        def productElement(n : Int) = throw new IndexOutOfBoundsException(n.toString)
    }

    /**
     * A class that extends `AnyVal` so needs special treatment.
     */
    case class AnyValClass(i : Int) extends AnyVal

    /**
     * A class that doesn't extend `AnyVal`.
     */
    case class ProdClass(i : Int)

    /**
     * A container for just `AnyVal` instances.
     */
    case class AnyValContainer(f1 : AnyValClass, f2 : AnyValClass)

    /**
     * A container for a mixture of normal and `AnyVal` instances.
     */
    case class MixedValContainer(f1 : AnyValClass, f2 : Int, f3 : ProdClass)

}

/**
 * Rewriting tests.
 */
class RewriterTests(val rewriter : Rewriter) extends KiamaTests with Generator {

    import org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree._
    import org.bitbucket.inkytonik.kiama.util.Comparison.optsame
    import RewriterTests._
    import rewriter.{fail => rwfail, test => rwtest, _}

    test("basic arithmetic evaluation") {
        val eval =
            rule[Exp] {
                case Add(Num(i), Num(j)) => Num(i + j)
                case Sub(Num(i), Num(j)) => Num(i - j)
                case Mul(Num(i), Num(j)) => Num(i * j)
                case Div(Num(i), Num(0)) => Num(0) // Hack
                case Div(Num(i), Num(j)) => Num(i / j)
                case Var(_)              => Num(3) // Hack
            }
        check((t : Exp) => everywherebu(eval)(t) == Some(Num(t.value)))
        check((t : Exp) => reduce(eval)(t) == Some(Num(t.value)))
    }

    test("issubterm: a term is a subterm of itself") {
        check((t : Stmt) => optsame(Some(t), issubterm((t, t))))
        check((t : Exp) => optsame(Some(t), issubterm((t, t))))
    }

    test("issubterm: random descendants are subterms") {
        val random = new scala.util.Random

        /*
         * Pick a random Term child of t, returning t if there are no
         * children or there are children but none of them are Terms.
         */
        def pickchild(t : Product) : Any = {
            val children = for (i <- 0 until t.productArity) yield t.productElement(i)
            if (children.length == 0)
                // No children, just use t itself
                t
            else {
                val termnum = random.nextInt(children.length)
                children(termnum)
            }
        }

        /*
         * Pick a random descendant of t (including possibly t).
         */
        def pickdesc(t : Any) : Any =
            t match {
                case p : Product =>
                    if (random.nextBoolean()) {
                        pickchild(p)
                    } else {
                        val child = pickchild(p)
                        if (child == t)
                            t
                        else
                            pickdesc(child)
                    }
                case _ =>
                    t
            }

        check((t : Stmt) => optsame(Some(t), issubterm((pickdesc(t), t))))
        check((t : Exp) => optsame(Some(t), issubterm((pickdesc(t), t))))
    }

    {
        val t = Add(Num(1), Num(2))

        test("issubterm: selected subterms - fail") {
            issubterm((Num(42), t)) should beFailure
        }

        test("issubterm: selected subterms - succeed sub") {
            issubterm((Num(1), t)) should beSomeOf(t)
        }

        test("issubterm: selected subterms - succeed self") {
            issubterm((t, t)) should beSomeOf(t)
        }

        test("issubterm: selected proper subterms - fail") {
            ispropersubterm((Num(42), t)) should beFailure
        }

        test("issubterm: selected proper subterms - succeed sub") {
            ispropersubterm((Num(1), t)) should beSomeOf(t)
        }

        test("issubterm: selected proper subterms - fail self") {
            ispropersubterm((t, t)) should beFailure
        }

        test("issuperterm: selected superterms - fail") {
            issuperterm((t, Num(42))) should beFailure
        }

        test("issuperterm: selected superterms - succeed sub") {
            issuperterm((t, Num(1))) should beSomeOf(t)
        }

        test("issuperterm: selected superterms - succeed self") {
            issuperterm((t, t)) should beSomeOf(t)
        }

        test("issuperterm: selected proper superterms - fail") {
            ispropersuperterm((t, Num(42))) should beFailure
        }

        test("issuperterm: selected proper superterms - succeed sub") {
            ispropersuperterm((t, Num(1))) should beSomeOf(t)
        }

        test("issuperterm: selected proper superterms - fail self") {
            ispropersuperterm((t, t)) should beFailure
        }
    }

    test("strategies that have no effect: identity") {
        check((t : Stmt) => optsame(Some(t), id(t)))
        check((t : Exp) => optsame(Some(t), id(t)))
    }

    test("strategies that have no effect: some terms to themselves") {
        val noopstmt = everywherebu(rule[Asgn] { case a => a })
        check((t : Stmt) => optsame(Some(t), noopstmt(t)))
        check((t : Exp) => optsame(Some(t), noopstmt(t)))

        val noopexp = everywherebu(rule[Num] { case n => n })
        check((t : Stmt) => optsame(Some(t), noopexp(t)))
        check((t : Exp) => optsame(Some(t), noopexp(t)))
    }

    test("strategies that fail immediately") {
        check((t : Stmt) => rwfail(t) == None)
        check((t : Exp) => rwfail(t) == None)
    }

    test("where: failure") {
        check((t : Exp) => where(rwfail)(t) == None)
    }

    test("where: identity") {
        check((t : Exp) => optsame(Some(t), where(id)(t)))
    }

    test("where restores the original term after succcess") {
        val r = rule[Num] { case Num(i) => Num(i + 1) }
        val s = where(r)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("test: failure") {
        check((t : Exp) => rwtest(rwfail)(t) == None)
    }

    test("test: identity") {
        check((t : Exp) => optsame(Some(t), rwtest(id)(t)))
    }

    test("test restores the original term after succcess") {
        val r = rule[Num] { case Num(i) => Num(i + 1) }
        val s = rwtest(r)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("leaf detection") {
        check((t : Exp) =>
            optsame(if (t.productArity == 0) Some(t) else None, isleaf(t)))
    }

    test("innernode detection") {
        check((t : Exp) =>
            optsame(if (t.productArity == 0) None else Some(t), isinnernode(t)))
    }

    test("terms as strategies") {
        check((t : Stmt, u : Exp) => optsame(Some(t), (build(t))(u)))
        check((t : Exp, u : Exp) => optsame(Some(t), (build(t))(u)))
        check((t : Stmt, u : Stmt) => optsame(Some(t), (build(t))(u)))
        check((t : Exp, u : Stmt) => optsame(Some(t), (build(t))(u)))
    }

    test("options as strategies") {
        check((t : Stmt, u : Exp) => optsame(Some(t), option(Some(t))(u)))
        check((u : Exp) => optsame(None, option(None)(u)))
    }

    test("term combinator") {
        check((t : Stmt) => (term(t))(t) == Some(t))
        check((t : Exp) => (term(t))(t) == Some(t))

        val t = Add(Num(1), Num(2))
        term(Num(1))(t) should beFailure
        term(Num(42))(t) should beFailure
    }

    {
        val num1 = Num(1)
        val num2 = Num(2)
        val num3 = Num(3)

        val e1 = Mul(num2, num3)
        val e2 = Add(num2, num3)

        test("conditional choice operator: identity") {
            (id < build(num1) + build(Num(2)))(e1) should beSomeOf(num1)
        }

        test("conditional choice operator: failure") {
            (rwfail < build(Num(1)) + build(num2))(e1) should beSomeOf(num2)
        }

        test("conditional choice operator: condition for just success or failure") {
            val ismulbytwo = rule[Mul] { case t @ Mul(Num(2), _) => t }
            val multoadd = rule[Exp] { case Mul(Num(2), x) => Add(x, x) }
            val num99 = Num(99)
            val error : Strategy = build(num99)
            val trans1 = ismulbytwo < multoadd + error
            (trans1)(e1) shouldBe Some(Add(num3, num3))
            (trans1)(e2) should beSomeOf(num99)
        }

        test("conditional choice operator: condition that transforms object") {
            val mulbytwotoadd = rule[Exp] { case t @ Mul(Num(2), x) => Add(x, x) }
            val num42 = Num(42)
            val add = rule[Exp] { case Add(_, _) => num42 }
            val trans2 = mulbytwotoadd < add + id
            (trans2)(e1) should beSomeOf(num42)
            (trans2)(e2) should beSomeOf(e2)
        }
    }

    test("strategies can return another strategy") {
        // Test expressions
        val e1 = Mul(Num(2), Num(5))
        val e2 = Add(Num(4), Num(5))

        // Single step passing
        val twotothree = rule[Num] { case Num(2) => Num(3) }
        val pass = rulefs[Num] { case Num(2) => twotothree }
        val passtd = everywhere(pass)
        (passtd)(e1) shouldBe Some(Mul(Num(3), (Num(5))))
        (passtd)(e2) should beSomeOf(e2)
    }

    {
        val e = Mul(Num(1), Add(Sub(Var("hello"), Num(2)), Var("harold")))
        val ee = Mul(Num(1), Add(Sub(Var("hello"), Num(2)), Var("harold")))

        test("a bottomup traversal applying identity returns the same term") {
            (bottomup(id))(e) should beSomeOf(e)
        }

        test("a bottomup traversal applying identity doesn't return term with same value") {
            (bottomup(id))(e) should not(beSomeOf(ee))
        }

        test("counting all terms using count") {
            val countall = count { case _ => 1 }
            countall(e) shouldBe 11
        }

        test("counting all terms using queryf") {
            var count = 0
            val countall = everywhere(queryf(_ => count = count + 1))
            countall(e) should beSomeOf(e)
            count shouldBe 11
        }

        test("counting all terms using a para") {
            val countfold =
                para[Int] {
                    case (t, cs) => 1 + cs.sum
                }
            countfold(e) shouldBe 11
        }

        test("counting all Num terms twice") {
            val countnum = count { case Num(_) => 2 }
            countnum(e) shouldBe 4
        }

        test("counting all Div terms") {
            val countdiv = count { case Div(_, _) => 1 }
            countdiv(e) shouldBe 0
        }

        test("counting all binary operator terms, with Muls twice") {
            val countbin = count {
                case Add(_, _) => 1
                case Sub(_, _) => 1
                case Mul(_, _) => 2
                case Div(_, _) => 1
            }
            countbin(e) shouldBe 4
        }

        {
            val ll1 = List(Num(1), Num(2), Num(3))
            val lr1 = List(Num(2), Num(3), Num(4))
            val ll2 = List(Num(1), Var("i"), Num(3))
            val ll3 = List(Num(1))

            val vl1 = Vector(Num(1), Num(2), Num(3))
            val vr1 = Vector(Num(2), Num(3), Num(4))
            val vl2 = Vector(Num(1), Var("i"), Num(3))
            val vl3 = Vector(Num(1))

            test("map fail over a nil list gives nil") {
                map(rwfail)(Nil) should beSomeOf(Nil)
            }

            test("map fail over a non-nil list fails") {
                map(rwfail)(ll1) should beFailure
            }

            test("map id over a nil list gives nil") {
                map(id)(Nil) should beSomeOf(Nil)
            }

            test("map id over a non-nil list gives that list") {
                map(id)(ll1) should beSomeOf(ll1)
            }

            test("map fail over an empty vector gives an empty vector") {
                map(rwfail)(Vector()) should beSomeOf(Vector())
            }

            test("map fail over a non-empty vector fails") {
                map(rwfail)(vl1) should beFailure
            }

            test("map id over an empty vector gives an empty vector") {
                map(id)(Vector()) should beSomeOf(Vector())
            }

            test("map id over a non-empty vector gives that vector") {
                map(id)(vl1) should beSomeOf(vl1)
            }

            {
                val inc = rule[Double] { case d => d + 1 }

                test("map double inc over a nil list gives nil") {
                    map(inc)(Nil) should beSomeOf(Nil)
                }

                test("map double inc over a non-nil list fails") {
                    map(inc)(ll1) should beFailure
                }

                test("map double inc over an empty vector gives an empty vector") {
                    map(inc)(Vector()) should beSomeOf(Vector())
                }

                test("map double inc over a non-empty vector fails") {
                    map(inc)(vl1) should beFailure
                }
            }

            {
                val isnum = rule[Num] { case n => n }

                test("map isnum over a nil list gives nil") {
                    map(isnum)(Nil) should beSomeOf(Nil)
                }

                test("map isnum over a list with one num succeeds with same list") {
                    map(isnum)(ll3) should beSomeOf(ll3)
                }

                test("map isnum over a list with more than one num succeeds with same list") {
                    map(isnum)(ll1) should beSomeOf(ll1)
                }

                test("map isnum over a list with non-num fails") {
                    map(isnum)(ll2) should beFailure
                }

                test("map isnum over an empty vector gives an empty vector") {
                    map(isnum)(Vector()) should beSomeOf(Vector())
                }

                test("map isnum over a vector with one num succeeds with same vector") {
                    map(isnum)(vl3) should beSomeOf(vl3)
                }

                test("map isnum over a vector with more than one num succeeds with same vector") {
                    map(isnum)(vl1) should beSomeOf(vl1)
                }

                test("map isnum over a vector with non-num fails") {
                    map(isnum)(vl2) should beFailure
                }
            }

            {
                val isnuminc = rule[Num] { case Num(i) => Num(i + 1) }

                test("map isnuminc over a nil list gives nil") {
                    map(isnuminc)(Nil) should beSomeOf(Nil)
                }

                test("map isnuminc over a list with only nums succeeds with incremented list") {
                    map(isnuminc)(ll1) shouldBe Some(lr1)
                }

                test("map isnuminc over a list with non-num fails") {
                    map(isnuminc)(ll2) should beFailure
                }

                test("map isnuminc over an empty vector gives an empty vector") {
                    map(isnuminc)(Vector()) should beSomeOf(Vector())
                }

                test("map isnuminc over a vector with only nums succeeds with incremented vector") {
                    map(isnuminc)(vl1) shouldBe Some(vr1)
                }

                test("map isnuminc over a vector with non-num fails") {
                    map(isnuminc)(vl2) should beFailure
                }
            }
        }

        {
            val r = Mul(Num(2), Add(Sub(Var("hello"), Num(3)), Var("harold")))
            val s = Mul(Num(2), Add(Sub(Var("hello"), Num(2)), Var("harold")))

            val double = rule[Double] { case d => d + 1 }

            test("rewriting leaf types: increment doubles - all, topdown") {
                (alltd(double))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: increment doubles - all, bottomup, same") {
                (allbu(double))(e) should beSomeOf(e)
            }

            test("rewriting leaf types: increment doubles - all, bottomup, not same") {
                (allbu(double))(e) should not(beSomeOf(ee))
            }

            test("rewriting leaf types: increment doubles - some, topdown") {
                (sometd(double))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: increment doubles - some, bottomup") {
                (somebu(double))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: increment doubles - one, topdown") {
                (oncetd(double))(e) shouldBe Some(s)
            }

            test("rewriting leaf types: increment doubles - one, bottomup") {
                (oncebu(double))(e) shouldBe Some(s)
            }
        }

        {
            val r = Mul(Num(1), Add(Sub(Var("olleh"), Num(2)), Var("dlorah")))
            val s = Mul(Num(1), Add(Sub(Var("olleh"), Num(2)), Var("harold")))

            val rev = rule[String] { case s => s.reverse }

            test("rewriting leaf types: reverse identifiers - all, topdown") {
                (alltd(rev))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: reverse identifiers - all, bottomup, same") {
                (allbu(rev))(e) should beSomeOf(e)
            }

            test("rewriting leaf types: reverse identifiers - all, bottomup, not same") {
                (allbu(rev))(e) should not(beSomeOf(ee))
            }

            test("rewriting leaf types: reverse identifiers - some, topdown") {
                (sometd(rev))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: reverse identifiers - some, bottomup") {
                (somebu(rev))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: reverse identifiers - one, topdown") {
                (oncetd(rev))(e) shouldBe Some(s)
            }

            test("rewriting leaf types: reverse identifiers - one, bottomup") {
                (oncebu(rev))(e) shouldBe Some(s)
            }
        }

        {
            val r = Mul(Num(2), Add(Sub(Var("olleh"), Num(2)), Var("dlorah")))
            val s = Mul(Num(2), Add(Sub(Var("hello"), Num(2)), Var("harold")))

            val evendoubleincrev =
                rule[Any] {
                    case i : Double if i < 2 => i + 1
                    case s : String          => s.reverse
                }

            test("rewriting leaf types: increment even doubles and reverse idn - all, topdown") {
                (alltd(evendoubleincrev))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: increment even doubles and reverse idn - all, bottomup, same") {
                (allbu(evendoubleincrev))(e) should beSomeOf(e)
            }

            test("rewriting leaf types: increment even doubles and reverse idn - all, bottomup, not same") {
                (allbu(evendoubleincrev))(e) should not(beSomeOf(ee))
            }

            test("rewriting leaf types: increment even doubles and reverse idn - some, topdown") {
                (sometd(evendoubleincrev))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: increment even doubles and reverse idn - some, bottomup") {
                (somebu(evendoubleincrev))(e) shouldBe Some(r)
            }

            test("rewriting leaf types: increment even doubles and reverse idn - one, topdown") {
                (oncetd(evendoubleincrev))(e) shouldBe Some(s)
            }

            test("rewriting leaf types: increment even doubles and reverse idn - one, bottomup") {
                (oncebu(evendoubleincrev))(e) shouldBe Some(s)
            }
        }
    }

    test("rewrite to increment an integer") {
        val inc = rule[Int] { case i => i + 1 }
        (inc)(3) shouldBe Some(4)
    }

    test("rewrite to a constant value") {
        val const = rulef(_ => 88)
        (const)(3) shouldBe Some(88)
    }

    test("rewrite failing to increment an integer with a double increment") {
        val inc = rule[Double] { case d => d + 1 }
        (inc)(3) should beFailure
    }

    {
        val incall = alltd(rule[Int] { case i => i + 1 })
        val incfirst = oncetd(rule[Int] { case i => i + 1 })
        val incodd = sometd(rule[Int] { case i if i % 2 != 0 => i + 1 })

        test("rewrite list: increment all numbers - non-empty") {
            (incall)(List(1, 2, 3)) shouldBe Some(List(2, 3, 4))
        }

        test("rewrite list: increment all numbers - empty") {
            (incall)(Nil) shouldBe Some(Nil)
        }

        test("rewrite list: increment first number - non-empty") {
            (incfirst)(List(1, 2, 3)) shouldBe Some(List(2, 2, 3))
        }

        test("rewrite list: increment first number - empty") {
            (incfirst)(Nil) should beFailure
        }

        test("rewrite list: increment odd numbers - succeed") {
            (incodd)(List(1, 2, 3)) shouldBe Some(List(2, 2, 4))
        }

        test("rewrite list: increment odd numbers - fail") {
            (incodd)(List(2, 4, 6)) should beFailure
        }

        val ll = List(List(1, 2), List(3), List(4, 5, 6))

        test("rewrite list: nested increment all numbers") {
            (incall)(ll) shouldBe Some(List(List(2, 3), List(4), List(5, 6, 7)))
        }

        test("rewrite list: nested increment first number") {
            (incfirst)(ll) shouldBe Some(List(List(2, 2), List(3), List(4, 5, 6)))
        }

        test("rewrite list: nested increment odd numbers - succeed") {
            (incodd)(ll) shouldBe Some(List(List(2, 2), List(4), List(4, 6, 6)))
        }

        test("rewrite list: nested increment odd numbers - fail") {
            (incodd)(List(List(2, 2), List(4), List(4, 6, 6))) should beFailure
        }

        test("rewrite vector: increment all numbers - non-empty") {
            (incall)(Vector(1, 2, 3)) shouldBe Some(Vector(2, 3, 4))
        }

        test("rewrite vector: increment all numbers - empty") {
            (incall)(Vector()) shouldBe Some(Vector())
        }

        test("rewrite vector: increment first number - non-empty") {
            (incfirst)(Vector(1, 2, 3)) shouldBe Some(Vector(2, 2, 3))
        }

        test("rewrite vector: increment first number - empty") {
            (incfirst)(Vector()) should beFailure
        }

        test("rewrite vector: increment odd numbers - succeed") {
            (incodd)(Vector(1, 2, 3)) shouldBe Some(Vector(2, 2, 4))
        }

        test("rewrite vector: increment odd numbers - fail") {
            (incodd)(Vector(2, 4, 6)) should beFailure
        }

        val vl = Vector(Vector(1, 2), Vector(3), Vector(4, 5, 6))

        test("rewrite vector: nested increment all numbers") {
            (incall)(vl) shouldBe Some(Vector(Vector(2, 3), Vector(4), Vector(5, 6, 7)))
        }

        test("rewrite vector: nested increment first number") {
            (incfirst)(vl) shouldBe Some(Vector(Vector(2, 2), Vector(3), Vector(4, 5, 6)))
        }

        test("rewrite vector: nested increment odd numbers - succeed") {
            (incodd)(vl) shouldBe Some(Vector(Vector(2, 2), Vector(4), Vector(4, 6, 6)))
        }

        test("rewrite vector: nested increment odd numbers - fail") {
            (incodd)(Vector(Vector(2, 2), Vector(4), Vector(4, 6, 6))) should beFailure
        }
    }

    test("same comparison of equal references yields true xxxx") {
        class Num(i : Int)
        val r = new Num(42)
        optsame(r, r) shouldBe true
    }

    test("same comparison of unequalt references yields false") {
        class Num(i : Int)
        val r1 = new Num(42)
        val r2 = new Num(42)
        optsame(r1, r2) shouldBe false
    }

    test("same comparison of equal non-references yields true") {
        optsame(42, 42) shouldBe true
    }

    test("same comparison of unequalt non-references yields false") {
        optsame(42, 43) shouldBe false
    }

    /**
     * The kind of comparison that is expected to be true for a test.  Equal
     * means use ==.  Same means the result must be the same reference or, if
     * the values are not references, use ==.  NotSame is the opposite of Same.
     */
    sealed abstract class Expecting
    case object Equal extends Expecting
    case object Same extends Expecting
    case object NotSame extends Expecting

    def travtest(basemsg : String, testmsg : String,
        eval : => Option[Any], expected : => Option[Any],
        expecting : Expecting = Equal) : Unit = {
        val msg = s"$basemsg - $testmsg, $expecting"
        test(msg) {
            expecting match {
                case Equal   => eval shouldBe expected
                case Same    => eval should beSameOptionAs(expected)
                case NotSame => eval should not(beSameOptionAs(expected))
            }
        }
    }

    {
        val l = List(Sub(Num(2), Var("one")), Add(Num(4), Num(5)), Var("two"))
        val ll = List(Sub(Num(2), Var("one")), Add(Num(4), Num(5)), Var("two"))
        val r = List(Sub(Num(0), Var("one")), Add(Num(0), Num(0)), Var("two"))
        val s = List(Sub(Num(0), Var("one")), Add(Num(4), Num(5)), Var("two"))

        val strat = rule[Double] { case _ => 0 }
        val basemsg = "rewrite list: doubles to zero in non-primitive list"

        travtest(basemsg, "all, topdown", alltd(strat)(l), Some(r))
        travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(l), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(ll), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(l), Some(r))
        travtest(basemsg, "some, bottomup", somebu(strat)(l), Some(r))
        travtest(basemsg, "one, topdown", oncetd(strat)(l), Some(s))
        travtest(basemsg, "one, bottomup", oncebu(strat)(l), Some(s))
    }

    {
        val l = Vector(Sub(Num(2), Var("one")), Add(Num(4), Num(5)), Var("two"))
        val ll = Vector(Sub(Num(2), Var("one")), Add(Num(4), Num(5)), Var("two"))
        val r = Vector(Sub(Num(0), Var("one")), Add(Num(0), Num(0)), Var("two"))
        val s = Vector(Sub(Num(0), Var("one")), Add(Num(4), Num(5)), Var("two"))

        val strat = rule[Double] { case _ => 0 }
        val basemsg = "rewrite vector: doubles to zero in non-primitive vector"

        travtest(basemsg, "all, topdown", alltd(strat)(l), Some(r))
        travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(l), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(ll), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(l), Some(r))
        travtest(basemsg, "some, bottomup", somebu(strat)(l), Some(r))
        travtest(basemsg, "one, topdown", oncetd(strat)(l), Some(s))
        travtest(basemsg, "one, bottomup", oncebu(strat)(l), Some(s))
    }

    {
        val v = Set(1, 5, 8, 9)
        val vv = Set(1, 5, 8, 9)

        val strat = rule[Int] { case i => i }
        val basemsg = "rewrite set: no change"

        travtest(basemsg, "all, topdown", alltd(strat)(v), Some(v), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(v), Some(v), Same)
        travtest(basemsg, "some, topdown", sometd(strat)(v), Some(v), Same)
        travtest(basemsg, "some, bottomup", somebu(strat)(v), Some(v), Same)
        travtest(basemsg, "one, topdown", oncetd(strat)(v), Some(v), Same)
        travtest(basemsg, "one, bottomup", oncebu(strat)(v), Some(v), Same)

        travtest(basemsg, "all, topdown", alltd(strat)(v), Some(vv), NotSame)
        travtest(basemsg, "all, bottomup", allbu(strat)(v), Some(vv), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(v), Some(vv), NotSame)
        travtest(basemsg, "some, bottomup", somebu(strat)(v), Some(vv), NotSame)
        travtest(basemsg, "one, topdown", oncetd(strat)(v), Some(vv), NotSame)
        travtest(basemsg, "one, bottomup", oncebu(strat)(v), Some(vv), NotSame)
    }

    {
        val r = Set(1, 5, 8, 9)
        val rr = Set(1, 5, 8, 9)
        val s = Set(2, 10, 16, 18)
        val t = Set(2, 5, 8, 9)

        val strat = rule[Int] { case i => i * 2 }
        val basemsg = "rewrite set: double value"

        travtest(basemsg, "all, topdown", alltd(strat)(r), Some(s))
        travtest(basemsg, "all, bottomup", allbu(strat)(r), Some(r), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(r), Some(rr), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(r), Some(s))
        travtest(basemsg, "some, bottomup", somebu(strat)(r), Some(s))
        travtest(basemsg, "one, topdown", oncetd(strat)(r), Some(t))
        travtest(basemsg, "one, bottomup", oncebu(strat)(r), Some(t))
    }

    {
        val m = Map("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map("one" -> 1, "two" -> 2, "three" -> 3)

        val strat = rule[String] { case s => s }
        val basemsg = "rewrite map: no change"

        travtest(basemsg, "all, topdown", alltd(strat)(m), Some(m), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(m), Same)
        travtest(basemsg, "some, topdown", sometd(strat)(m), Some(m), Same)
        travtest(basemsg, "some, bottomup", somebu(strat)(m), Some(m), Same)
        travtest(basemsg, "one, topdown", oncetd(strat)(m), Some(m), Same)
        travtest(basemsg, "one, bottomup", oncebu(strat)(m), Some(m), Same)

        travtest(basemsg, "all, topdown", alltd(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "some, bottomup", somebu(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "one, topdown", oncetd(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "one, bottomup", oncebu(strat)(m), Some(mm), NotSame)
    }

    {
        val m = Map("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map("eno" -> 1, "owt" -> 2, "eerht" -> 3)
        val s = Map("eno" -> 1, "two" -> 2, "three" -> 3)

        val strat = rule[String] { case s => s.reverse }
        val basemsg = "rewrite map: reverse keys"

        travtest(basemsg, "all, topdown", alltd(strat)(m), Some(r))
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(m), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(m), Some(r))
        travtest(basemsg, "some, bottomup", somebu(strat)(m), Some(r))
        travtest(basemsg, "one, topdown", oncetd(strat)(m), Some(s))
        travtest(basemsg, "one, bottomup", oncebu(strat)(m), Some(s))
    }

    {
        val m = Map("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map("one" -> 2, "two" -> 3, "three" -> 4)
        val s = Map("one" -> 2, "two" -> 2, "three" -> 3)

        val strat = rule[Int] { case i => i + 1 }
        val basemsg = "rewrite map: increment values"

        travtest(basemsg, "all, topdown", alltd(strat)(m), Some(r))
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(m), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(m), Some(r))
        travtest(basemsg, "some, bottomup", somebu(strat)(m), Some(r))
        travtest(basemsg, "one, topdown", oncetd(strat)(m), Some(s))
        travtest(basemsg, "one, bottomup", oncebu(strat)(m), Some(s))
    }

    {
        val m = Map("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map("eno" -> 2, "owt" -> 3, "eerht" -> 4)
        val s = Map("eno" -> 1, "two" -> 2, "three" -> 3)

        val basemsg = "rewrite map: reverse keys and increment values"
        val strat = rule[Any] {
            case s : String => s.reverse
            case i : Int    => i + 1
        }

        travtest(basemsg, "all, topdown", alltd(strat)(m), Some(r))
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(m), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(m), Some(r))
        travtest(basemsg, "some, bottomup", somebu(strat)(m), Some(r))
        travtest(basemsg, "one, topdown", oncetd(strat)(m), Some(s))
        travtest(basemsg, "one, bottomup", oncebu(strat)(m), Some(s))
    }

    {
        val m = Map(1 -> 2, 3 -> 4, 5 -> 6)
        val mm = Map(1 -> 2, 3 -> 4, 5 -> 6)
        val r = Map(2 -> 4, 4 -> 8, 6 -> 12)
        val s = Map(2 -> 4, 3 -> 4, 5 -> 6)

        val basemsg = "rewrite map: increment key and double value"
        val strat = rule[(Int, Int)] { case (k, v) => (k + 1, v * 2) }

        travtest(basemsg, "all, topdown", alltd(strat)(m), Some(r))
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(m), Same)
        travtest(basemsg, "all, bottomup", allbu(strat)(m), Some(mm), NotSame)
        travtest(basemsg, "some, topdown", sometd(strat)(m), Some(r))
        travtest(basemsg, "some, bottomup", somebu(strat)(m), Some(r))
        travtest(basemsg, "one, topdown", oncetd(strat)(m), Some(s))
        travtest(basemsg, "one, bottomup", oncebu(strat)(m), Some(s))
    }

    {
        // Maps from sets to their sizes, on init size is always zero
        val m1 = Map(Set(1, 3) -> 0, Set(2, 4, 6) -> 0)
        val m2 = Map(Set(12, 16) -> 0, Set(23) -> 0)

        // Collection of the maps
        val l = Vector(m1, m2)
        val ll = Vector(
            Map(Set(1, 3) -> 0, Set(2, 4, 6) -> 0),
            Map(Set(12, 16) -> 0, Set(23) -> 0)
        )

        {
            val r = Vector(
                Map(Set(2, 4) -> 1, Set(3, 5, 7) -> 1),
                Map(Set(13, 17) -> 1, Set(24) -> 1)
            )
            val s = Vector(
                Map(Set(2, 3) -> 0, Set(2, 4, 6) -> 0),
                Map(Set(12, 16) -> 0, Set(23) -> 0)
            )

            val basemsg = "rewrite set: heterogeneous collection: inc integers"
            val strat = rule[Int] { case i => i + 1 }

            travtest(basemsg, "all, topdown", alltd(strat)(l), Some(r))
            travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(l), Same)
            travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(ll), NotSame)
            travtest(basemsg, "some, topdown", sometd(strat)(l), Some(r))
            travtest(basemsg, "some, bottomup", somebu(strat)(l), Some(r))
            travtest(basemsg, "one, topdown", oncetd(strat)(l), Some(s))
            travtest(basemsg, "one, bottomup", oncebu(strat)(l), Some(s))
        }

        {
            val r = Vector(
                Map(Set(1, 3) -> 2, Set(2, 4, 6) -> 3),
                Map(Set(12, 16) -> 2, Set(23) -> 1)
            )
            val s = Vector(
                Map(Set(1, 3) -> 2, Set(2, 4, 6) -> 0),
                Map(Set(12, 16) -> 0, Set(23) -> 0)
            )

            val basemsg = "rewrite set: heterogeneous collection: set to size"
            val strat = rule[(Set[_], Int)] { case (s, _) => (s, s.size) }

            travtest(basemsg, "all, topdown", alltd(strat)(l), Some(r))
            travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(l), Same)
            travtest(basemsg, "all, bottomup", allbu(strat)(l), Some(ll), NotSame)
            travtest(basemsg, "some, topdown", sometd(strat)(l), Some(r))
            travtest(basemsg, "some, bottomup", somebu(strat)(l), Some(r))
            travtest(basemsg, "one, topdown", oncetd(strat)(l), Some(s))
            travtest(basemsg, "one, bottomup", oncebu(strat)(l), Some(s))
        }
    }

    {
        val incnum = rule[Num] { case Num(i) => Num(i + 1) }
        val inczerothchild = child(0, incnum)
        val incfirstchild = child(1, incnum)
        val incsecondchild = child(2, incnum)
        val incthirdchild = child(3, incnum)
        val incallsecondchild = alltd(incsecondchild)

        test("rewrite by child index: inc zeroth child - fail") {
            inczerothchild(Add(Num(2), Num(3))) should beFailure
        }

        test("rewrite by child index: inc first child - fail") {
            incfirstchild(Num(2)) should beFailure
        }

        test("rewrite by child index: inc first child - succeed, one child, one level") {
            incfirstchild(Neg(Num(2))) shouldBe Some(Neg(Num(3)))
        }

        test("rewrite by child index: inc first child - succeed, two children, one level") {
            incfirstchild(Add(Num(2), Num(3))) shouldBe Some(Add(Num(3), Num(3)))
        }

        test("rewrite by child index: inc second child - fail") {
            incsecondchild(Num(2)) should beFailure
        }

        test("rewrite by child index: inc second child - succeed, one level") {
            incsecondchild(Add(Num(2), Num(3))) shouldBe Some(Add(Num(2), Num(4)))
        }

        test("rewrite by child index: inc third child - fail, one level") {
            incthirdchild(Add(Num(2), Num(3))) should beFailure
        }

        test("rewrite by child index: inc second child - succeed, multi-level") {
            incallsecondchild(Sub(Add(Num(2), Num(3)), Mul(Num(4), Num(5)))) shouldBe Some(Sub(Add(Num(2), Num(4)), Mul(Num(4), Num(6))))
        }
    }

    {
        // The type used here should be a finite immutable sequence that is not
        // implemented using case classes or other Products (which rules out lists).

        val incint = rule[Int] { case i => i + 1 }
        val inczerothchild = child(0, incint)
        val incfirstchild = child(1, incint)
        val incsecondchild = child(2, incint)
        val incallsecondchild = alltd(incsecondchild)

        val l1 = Vector()
        val l2 = Vector(1)
        val l3 = Vector(1, 2, 3, 4)

        test("rewrite linkedlist by child index: inc zeroth child - fail, empty") {
            inczerothchild(l1) should beFailure
        }

        test("rewrite linkedlist by child index: inc first child - fail, empty") {
            incfirstchild(l1) should beFailure
        }

        test("rewrite linkedlist by child index: inc first child - succeed, singleton") {
            incfirstchild(l2) shouldBe Some(Vector(2))
        }

        test("rewrite linkedlist by child index: inc second child - fail, singleton") {
            incsecondchild(l2) should beFailure
        }

        test("rewrite linkedlist by child index: inc zeroth child - fail, multiple") {
            inczerothchild(l3) should beFailure
        }

        test("rewrite linkedlist by child index: inc first child - succeed, multiple") {
            incfirstchild(l3) shouldBe Some(Vector(2, 2, 3, 4))
        }

        test("rewrite linkedlist by child index: inc second child - succeed, one level") {
            incsecondchild(l3) shouldBe Some(Vector(1, 3, 3, 4))
        }

        test("rewrite linkedlist by child index: inc second child - succeed, multi-level") {
            incallsecondchild(Vector(Vector(1), Vector(3, 4, 5), Vector(6, 7))) shouldBe Some(Vector(Vector(1), Vector(3, 5, 5), Vector(6, 8)))
        }
    }

    {
        // { i = 10; count = 0; while (i) { count = count + 1; i = 1 + i; } }
        val p =
            Seqn(Vector(
                Asgn(Var("i"), Num(10)),
                Asgn(Var("count"), Num(0)),
                While(
                    Var("i"),
                    Seqn(Vector(
                        Asgn(Var("count"), Add(Var("count"), Num(1))),
                        Asgn(Var("i"), Add(Num(1), Var("i")))
                    ))
                )
            ))

        // { i = 0; count = 0; while (i) { count = bob + 1; i = 0 + i; } }
        val q =
            Seqn(Vector(
                Asgn(Var("i"), Num(0)),
                Asgn(Var("count"), Num(0)),
                While(
                    Var("i"),
                    Seqn(Vector(
                        Asgn(Var("count"), Add(Var("bob"), Num(1))),
                        Asgn(Var("i"), Add(Num(0), Var("i")))
                    ))
                )
            ))

        val incint = rule[Int] { case i => i + 1 }
        val clearvector = rule[Vector[_]] { case _ => Vector() }
        val zeronumsbreakadds =
            alltd(Num(rule[Double] { case _ => 0 }) +
                Add(rule[Var] { case _ => Var("bob") }, id))

        test("rewrite by congruence: top-level wrong congruence") {
            Num(incint)(p) should beFailure
        }

        test("rewrite by congruence: top-level correct congruence") {
            Seqn(clearvector)(p) shouldBe Some(Seqn(Vector()))
        }

        test("rewrite by congruence: multi-level") {
            zeronumsbreakadds(p) shouldBe Some(q)
        }
    }

    test("debug strategy produces the expected message and result") {
        import org.bitbucket.inkytonik.kiama.util.StringEmitter
        val e = new StringEmitter
        val s = debug("hello there: ", e)
        val t = Asgn(Var("i"), Add(Num(1), Var("i")))
        s(t) should beSomeOf(t)
        e.result() shouldBe s"hello there: $t\n"
    }

    {
        import org.bitbucket.inkytonik.kiama.util.StringEmitter
        val r = rule[Asgn] { case Asgn(l, r) => Asgn(l, Num(42)) }

        test("log strategy produces the expected message and result on success") {
            val e = new StringEmitter
            val s = log(r, "test log ", e)
            val t = Asgn(Var("i"), Add(Num(1), Var("i")))
            val u = Asgn(Var("i"), Num(42))
            s(t) shouldBe Some(u)
            e.result() shouldBe s"test log $t succeeded with $u\n"
        }

        test("log strategy produces the expected message and result on failure") {
            val e = new StringEmitter
            val s = log(r, "test log ", e)
            val t = Add(Num(1), Var("i"))
            s(t) should beFailure
            e.result() shouldBe s"test log $t failed\n"
        }

        test("logfail strategy produces no message but the right result on success") {
            val e = new StringEmitter
            val s = logfail(r, "test log ", e)
            val t = Asgn(Var("i"), Add(Num(1), Var("i")))
            val u = Asgn(Var("i"), Num(42))
            s(t) shouldBe Some(u)
            e.result() shouldBe ""
        }

        test("logfail strategy produces the expected message and result on failure") {
            val e = new StringEmitter
            val s = logfail(r, "test log ", e)
            val t = Add(Num(1), Var("i"))
            s(t) should beFailure
            e.result() shouldBe s"test log $t failed\n"
        }
    }

    test("rewrite returns the original term when the strategy fails") {
        val t = Asgn(Var("i"), Add(Num(1), Var("i")))
        rewrite(rwfail)(t) should be theSameInstanceAs t
    }

    test("rewrite returns the strategy result when the strategy succeeds") {
        val t = Asgn(Var("i"), Add(Num(1), Var("i")))
        val s = everywhere(rule[Var] { case _ => Var("hello") })
        Some(rewrite(s)(t)) shouldBe s(t)
    }

    test("a memo strategy returns the previous result without re-evaluating") {
        val t = Asgn(Var("i"), Add(Num(1), Var("i")))
        var count = 0
        val s = memo(everywhere(rule[Var] {
            case _ =>
                count = count + 1;
                Var(s"i$count")
        }))
        val r = Some(Asgn(Var("i1"), Add(Num(1), Var("i2"))))
        count shouldBe 0
        s(t) shouldBe r
        count shouldBe 2
        s(t) shouldBe r
        count shouldBe 2
    }

    {
        val t = SingleCaseObject

        test("a copy of a singleton case object doesn't copy") {
            val u = copy(t)
            u shouldBe t
            u should be theSameInstanceAs t
        }

        test("a dup of a singleton case object doesn't dup") {
            val u = dup(t, Array())
            u shouldBe t
            u should be theSameInstanceAs t
        }
    }

    {
        val t = SingleObject

        test("a copy of a singleton object doesn't copy") {
            val u = copy(t)
            u shouldBe t
            u should be theSameInstanceAs t
        }

        test("a dup of a singleton object doesn't dup") {
            val u = dup(t, Array())
            u shouldBe t
            u should be theSameInstanceAs t
        }
    }

    {
        val t = Nil

        test("a copy of Nil doesn't copy") {
            val u = copy(t)
            u shouldBe t
            u should be theSameInstanceAs t
        }

        test("a dup of Nil doesn't dup") {
            val u = dup(t, Array())
            u shouldBe t
            u should be theSameInstanceAs t
        }
    }

    {
        val t = Null()

        test("a copy of a no-children instance copies") {
            val u = copy(t)
            u shouldBe t
            u should not(be theSameInstanceAs t)
        }

        test("a dup of a no-children instance dups") {
            val u = dup(t, Array())
            u shouldBe t
            u should not(be theSameInstanceAs t)
        }
    }

    {
        val t = Var("i")

        test("a copy of a node with a child copies") {
            val u = copy(t)
            u shouldBe t
            u should not(be theSameInstanceAs t)
        }

        test("a dup of a node with a child dups") {
            val u = dup(t, Array("j"))
            u shouldBe Var("j")
            u should not(be theSameInstanceAs t)
        }
    }

    {
        val t = Add(Num(1), Num(2))

        test("a copy of a node with multiple children copies") {
            val u = copy(t)
            u shouldBe t
            u should not(be theSameInstanceAs t)
        }

        test("a dup of a node with multiple children dups") {
            val u = dup(t, Array(Num(3), Num(4)))
            u shouldBe Add(Num(3), Num(4))
            u should not(be theSameInstanceAs t)
        }
    }

    test("an illegal dup throws an appropriate exception") {
        val t = Asgn(Var("i"), Add(Num(1), Var("i")))
        val i = intercept[RuntimeException] {
            dup(t, Array(Num(42), Num(99)))
        }
        val base = "dup illegal arguments"
        val method = s"public org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree$$Asgn"
        val arg1type = s"org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree$$Var"
        val arg2type = s"org.bitbucket.inkytonik.kiama.example.imperative.ImperativeTree$$Exp"
        val error = "got (Num(42.0),Num(99.0))"
        val hint = "Common cause: term classes are nested in another class, move them to the top level"
        val msg = "%s: %s(%s,%s) %s\n%s".format(base, method, arg1type, arg2type, error, hint)
        i.getMessage shouldBe msg
    }

    test("repeat on failure succeeds") {
        val s = repeat(rwfail)
        val t = Num(10)
        s(t) should beSomeOf(t)
    }

    test("repeat of non-failure works") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val s = repeat(r)
        s(Num(1)) shouldBe Some(Num(10))
    }

    test("repeat with a final strategy on failure applies the final strategy") {
        val f = rule[Num] {
            case Num(10) => Num(20)
        }
        val s = repeat(rwfail, f)
        s(Num(10)) shouldBe Some(Num(20))
    }

    test("repeat with a final strategy works") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val f = rule[Num] {
            case Num(10) => Num(20)
        }
        val s = repeat(r, f)
        s(Num(1)) shouldBe Some(Num(20))
    }

    test("repeat with a final failure fails") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val s = repeat(r, rwfail)
        s(Num(1)) should beFailure
    }

    test("repeat1 on failure fails") {
        val s = repeat1(rwfail)
        s(Num(10)) should beFailure
    }

    test("repeat1 of non-failure works") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val s = repeat1(r)
        s(Num(1)) shouldBe Some(Num(10))
    }

    test("repeat1 with a final strategy on failure doesn't apply the final strategy") {
        val f = rule[Num] {
            case Num(10) => Num(20)
        }
        val s = repeat1(rwfail, f)
        s(Num(10)) should beFailure
    }

    test("repeat1 with a final strategy works") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val f = rule[Num] {
            case Num(10) => Num(20)
        }
        val s = repeat1(r, f)
        s(Num(1)) shouldBe Some(Num(20))
    }

    test("repeat1 with a final failure fails") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val s = repeat1(r, rwfail)
        s(Num(1)) should beFailure
    }

    test("zero repeat of failure is identity") {
        val s = repeat(rwfail, 0)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("non-zero repeat of failure fails") {
        val s = repeat(rwfail, 4)
        s(Num(1)) should beFailure
    }

    test("zero repeat of non-failure is identity") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val s = repeat(r, 0)
        val t = Num(1)
        s(t) shouldBe Some(t)
    }

    test("non-zero repeat of non-failure is repeated correct number of times") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val s = repeat(r, 4)
        s(Num(1)) shouldBe Some(Num(5))
    }

    test("repeatuntil on failure fails") {
        val f = rule[Num] {
            case Num(10) => Num(20)
        }
        val s = repeatuntil(rwfail, f)
        s(Num(1)) should beFailure
    }

    test("repeatuntil on non-failure works") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i + 1)
        }
        val f = rule[Num] {
            case Num(10) => Num(20)
        }
        val s = repeatuntil(r, f)
        s(Num(1)) shouldBe Some(Num(20))
    }

    test("loop on failure is identity") {
        val f = rule[Num] {
            case Num(1) => Num(2)
        }
        val s = loop(rwfail, f)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("loop on non-failure with initially false condition is identity") {
        val r = rule[Num] {
            case Num(i) if i > 10 => Num(i)
        }
        val f = rule[Num] {
            case Num(1) => Num(2)
        }
        val s = loop(r, f)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("loop on failure with initially true condition is identity") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i)
        }
        val s = loop(r, rwfail)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("loop on non-failure with initially true condition works") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i)
        }
        val f = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val s = loop(r, f)
        s(Num(1)) shouldBe Some(Num(10))
    }

    test("loopnot on succeess is identity") {
        val f = rule[Num] {
            case Num(1) => Num(2)
        }
        val s = loopnot(id, f)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("loopnot on non-failure with initially true condition is identity") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i)
        }
        val f = rule[Num] {
            case Num(1) => Num(2)
        }
        val s = loopnot(r, f)
        val t = Num(1)
        s(t) shouldBe Some(t)
    }

    test("loopnot on failure with initially false condition fails") {
        val r = rule[Num] {
            case Num(i) if i >= 10 => Num(i + 1)
        }
        val s = loopnot(r, rwfail)
        s(Num(1)) should beFailure
    }

    test("loopnot on non-failure with initially false condition works") {
        val r = rule[Num] {
            case Num(i) if i >= 10 => Num(i)
        }
        val f = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val s = loopnot(r, f)
        s(Num(1)) shouldBe Some(Num(10))
    }

    test("doloop on failure applies once") {
        val f = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val s = doloop(f, rwfail)
        s(Num(1)) shouldBe Some(Num(2))
    }

    test("doloop on non-failure with initially false condition applies once") {
        val r = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val f = rule[Num] {
            case Num(i) if i >= 10 => Num(i)
        }
        val s = doloop(r, f)
        s(Num(1)) shouldBe Some(Num(2))
    }

    test("doloop on failure with initially true condition is failure") {
        val f = rule[Num] {
            case Num(i) if i < 10 => Num(i)
        }
        val s = doloop(rwfail, f)
        s(Num(1)) should beFailure
    }

    test("doloop on non-failure with initially true condition works") {
        val r = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val f = rule[Num] {
            case Num(i) if i < 10 => Num(i)
        }
        val s = doloop(r, f)
        s(Num(1)) shouldBe Some(Num(10))
    }

    test("loopiter with failure init fails") {
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i)
        }
        val f = rule[Num] {
            case Num(1) => Num(2)
        }
        val s = loopiter(rwfail, r, f)
        s(Num(1)) should beFailure
    }

    test("loopiter with succeeding init and initially true condition works") {
        val i = rule[Num] {
            case Num(100) => Num(1)
        }
        val r = rule[Num] {
            case Num(i) if i < 10 => Num(i)
        }
        val f = rule[Num] {
            case Num(1) => Num(2)
        }
        val s = loopiter(i, r, f)
        s(Num(100)) shouldBe Some(Num(1))
    }

    test("loopiter with succeeding init and initially false condition works") {
        val i = rule[Num] {
            case Num(100) => Num(1)
        }
        val r = rule[Num] {
            case Num(i) if i >= 10 => Num(i)
        }
        val f = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val s = loopiter(i, r, f)
        s(Num(100)) shouldBe Some(Num(10))
    }

    test("counting loopiter is identity if there is nothing to count") {
        val r = (i : Int) =>
            rule[Num] {
                case Num(j) => Num(i + j)
            }
        val s = loopiter(r, 10, 1)
        val t = Num(1)
        s(t) should beSomeOf(t)
    }

    test("counting loopiter counts correctly") {
        var count = 0
        val r = (i : Int) =>
            rule[Num] {
                case Num(j) =>
                    count = count + i
                    Num(j + 1)
            }
        val s = loopiter(r, 1, 10)
        s(Num(1)) shouldBe Some(Num(11))
        count shouldBe 55
    }

    test("breadthfirst traverses in correct order") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        var l = Vector[Double]()
        val r = rule[Any] {
            case n @ Num(i) =>
                l = l :+ i
                n
            case n => n
        }
        val s = breadthfirst(r)
        s(t) should beSomeOf(t)
        l shouldBe Vector(3, 1, 2, 4, 5)
    }

    test("leaves with a failing leaf detector succeeds but doesn't collect anything") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        var sum = 0.0
        val r = rule[Exp] {
            case n @ Num(i) =>
                sum = sum + i
                n
            case n => n
        }
        val s = leaves(r, rwfail)
        s(t) shouldBe Some(t)
        sum shouldBe 0
    }

    test("leaves with a non-failing leaf detector succeeds and collects correctly") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        var sum = 0.0
        val r = rule[Num] {
            case Num(i) =>
                sum = sum + i
                Num(i)
        }
        val l = rule[Num] {
            case Num(i) if i % 2 != 1 => Num(i)
        }
        val s = leaves(r, l)
        s(t) shouldBe Some(t)
        sum shouldBe 6
    }

    test("skipping leaves with a non-failing leaf detector succeeds and collects correctly") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        var sum = 0.0
        val r = rule[Num] {
            case Num(i) =>
                sum = sum + i
                Num(i)
        }
        val l = rule[Num] {
            case Num(i) if i % 2 != 0 => Num(i)
        }
        val x = (y : Strategy) => rule[Sub] { case n => n }
        val s = leaves(r, l, x)
        s(t) shouldBe Some(t)
        sum shouldBe 4
    }

    {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Add(Var("1.0"), Var("2.0")), Var("3.0")), Sub(Var("4.0"), Var("5.0")))

        test("innermost visits the correct nodes in the correct order") {
            var l = Vector[Double]()
            val r = rule[Exp] {
                case Num(i) =>
                    l = l :+ i
                    Var(i.toString)
            }
            val s = innermost(r)
            s(t) shouldBe Some(u)
            l shouldBe Vector(1, 2, 3, 4, 5)
        }

        test("innermost2 visits the correct node") {
            var l = Vector[Double]()
            val r = rule[Exp] {
                case Num(i) =>
                    l = l :+ i
                    Var(i.toString)
            }
            val s = innermost2(r)
            s(t) shouldBe Some(u)
            l shouldBe Vector(1, 2, 3, 4, 5)
        }

    }

    test("downup (one arg version) visits the correct frontier") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(1), Num(2))), Sub(Num(4), Num(5)))
        val d = rule[Any] {
            case Add(l, r @ Num(3)) => Add(r, l)
            case Sub(l, r)          => Sub(r, l)
            case n                  => n
        }
        val s = downup(d)
        s(t) shouldBe Some(u)
    }

    test("downup (two arg version) visits the correct frontier") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(1), Num(2))), Sub(Num(8), Num(9)))
        val d = rule[Any] {
            case Add(l, r @ Num(3)) => Add(r, l)
            case Sub(l, r)          => Sub(r, l)
            case n                  => n
        }
        val e = rule[Any] {
            case Sub(l, r) => Sub(Num(8), Num(9))
            case n         => n
        }
        val s = downup(d, e)
        s(t) shouldBe Some(u)
    }

    test("somedownup visits the correct frontier") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Add(Num(3), Num(4)), Num(3)), Sub(Num(2), Num(3)))
        val d = rule[Exp] {
            case Add(Num(l), Num(r)) => Add(Num(l + 1), Num(r + 1))
            case Sub(Num(l), Num(r)) => Sub(Num(l - 1), Num(r - 1))
            case n : Mul             => n
        }
        val s = somedownup(d)
        s(t) shouldBe Some(u)
    }

    test("downupS (two arg version) visits the correct frontier") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(1), Num(2))), Sub(Num(4), Num(5)))
        val d = rule[Any] {
            case Add(l, r @ Num(3)) => Add(r, l)
            case Sub(l, r)          => Sub(r, l)
            case n                  => n
        }
        def f(y : => Strategy) : Strategy =
            rule[Add] {
                case n @ Add(_, Num(3)) => n
            }
        val s = downupS(d, f)
        s(t) shouldBe Some(u)
    }

    test("downupS (three arg version) visits the correct frontier") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(1), Num(2))), Sub(Num(8), Num(9)))
        val d = rule[Any] {
            case Add(l, r @ Num(3)) => Add(r, l)
            case Sub(l, r)          => Sub(r, l)
            case n                  => n
        }
        val e = rule[Any] {
            case Sub(l, r) => Sub(Num(8), Num(9))
            case n         => n
        }
        def f(y : => Strategy) : Strategy =
            rule[Add] {
                case n @ Add(_, Num(3)) => n
            }
        val s = downupS(d, e, f _)
        s(t) shouldBe Some(u)
    }

    test("alldownup2 visits the correct frontier") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Mul(Num(3), Add(Num(1), Num(2))), Sub(Num(5), Num(4)))
        val d = rule[Exp] {
            case Add(l, r @ Num(3)) => Add(r, l)
            case Sub(l, r)          => Sub(r, l)
        }
        val e = rule[Any] {
            case Add(l, r) => Mul(l, r)
            case n         => n
        }
        val s = alldownup2(d, e)
        s(t) shouldBe Some(u)
    }

    test("topdownS stops at the right spots") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(1), Num(2))), Sub(Num(5), Num(4)))
        val d = rule[Any] {
            case Add(l, r) => Add(r, l)
            case Sub(l, r) => Sub(r, l)
            case n         => n
        }
        def f(y : => Strategy) : Strategy =
            rule[Add] {
                case n @ Add(Num(3), _) => n
            }
        val s = topdownS(d, f)
        s(t) shouldBe Some(u)
    }

    test("topdownS with no stopping doesn't stop") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(2), Num(1))), Sub(Num(5), Num(4)))
        val d = rule[Any] {
            case Add(l, r) => Add(r, l)
            case Sub(l, r) => Sub(r, l)
            case n         => n
        }
        val s = topdownS(d, dontstop)
        s(t) shouldBe Some(u)
    }

    test("bottomupS stops at the right spots") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(1), Num(2))), Sub(Num(5), Num(4)))
        val d = rule[Any] {
            case Add(l, r) => Add(r, l)
            case Sub(l, r) => Sub(r, l)
            case n         => n
        }
        def f(y : => Strategy) : Strategy =
            rule[Add] {
                case n @ Add(_, Num(3)) => n
            }
        val s = bottomupS(d, f)
        s(t) shouldBe Some(u)
    }

    test("bottomupS with no stopping doesn't stop") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(3), Add(Num(2), Num(1))), Sub(Num(5), Num(4)))
        val d = rule[Any] {
            case Add(l, r) => Add(r, l)
            case Sub(l, r) => Sub(r, l)
            case n         => n
        }
        val s = bottomupS(d, dontstop)
        s(t) shouldBe Some(u)
    }

    test("manybu applies the strategy in the right order and right number of times") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Add(Num(12), Num(11)), Num(10)), Sub(Num(4), Num(5)))
        var count = 13
        val d = rule[Num] {
            case _ if count > 10 =>
                count = count - 1
                Num(count)
        }
        val s = manybu(d)
        s(t) shouldBe Some(u)
    }

    test("manytd applies the strategy in the right order and right number of times") {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Num(11), Add(Num(2), Num(1))), Sub(Num(4), Num(5)))
        var count = 13
        val d = rule[Exp] {
            case Num(i) if count > 10 =>
                count = count - 1
                Num(count)
            case Add(l, r) if count > 10 =>
                count = count - 1
                Add(r, l)
        }
        val s = manytd(d)
        s(t) shouldBe Some(u)
    }

    test("alltdfold can be used to evaluate an expression") {
        // ((1 + 2) + 3) * (4 - 5) = -6
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val d = rule[Num] { case n => n }
        val e = rule[Exp] {
            case Add(Num(i), Num(j)) => Num(i + j)
            case Sub(Num(i), Num(j)) => Num(i - j)
            case Mul(Num(i), Num(j)) => Num(i * j)
        }
        val s = alltdfold(d, e)
        s(t) shouldBe Some(Num(-6))
    }

    test("restore restores when the strategy fails") {
        val t = Add(Num(1), Num(2))
        var count = 0
        val d = rule[Exp] {
            case n => count = count + 1; n
        } <* rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val e = rule[Exp] {
            case n => count = count - 1; n
        }
        val s = restore(d, e)
        s(t) should beFailure
        count shouldBe 0
    }

    test("restore doesn't restore when the strategy succeeds") {
        val t = Add(Num(1), Num(2))
        var count = 0
        val d = rule[Exp] {
            case n => count = count + 1; n
        }
        val e = rule[Exp] {
            case n => count = count - 1; n
        }
        val s = restore(d, e)
        s(t) should beSomeOf(t)
        count shouldBe 1
    }

    test("restorealways restores when the strategy fails") {
        val t = Add(Num(1), Num(2))
        var count = 0
        val d = rule[Exp] {
            case n => count = count + 1; n
        } <* rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val e = rule[Exp] {
            case n => count = count - 1; n
        }
        val s = restorealways(d, e)
        s(t) should beFailure
        count shouldBe 0
    }

    test("restorealways restores when the strategy succeeds") {
        val t = Add(Num(1), Num(2))
        var count = 0
        val d = rule[Exp] {
            case n => count = count + 1; n
        }
        val e = rule[Exp] {
            case n => count = count - 1; n
        }
        val s = restorealways(d, e)
        s(t) should beSomeOf(t)
        count shouldBe 0
    }

    test("lastly applies the second strategy when the first strategy fails") {
        val t = Add(Num(1), Num(2))
        var count = 0
        val d = rule[Exp] {
            case n => count = count + 1; n
        } <* rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val e = rule[Exp] {
            case n => count = count - 1; n
        }
        val s = lastly(d, e)
        s(t) should beFailure
        count shouldBe 0
    }

    test("lastly applies the second strategy when the first strategy succeeds") {
        val t = Add(Num(1), Num(2))
        var count = 0
        val d = rule[Exp] {
            case n => count = count + 1; n
        }
        val e = rule[Exp] {
            case n => count = count - 1; n
        }
        val s = lastly(d, e)
        s(t) should beSomeOf(t)
        count shouldBe 0
    }

    test("ior applies second strategy if first strategy fails") {
        val t = Add(Num(1), Num(2))
        val u = Add(Num(2), Num(1))
        val d = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val e = rule[Add] {
            case Add(l, r) => Add(r, l)
        }
        val s = ior(d, e)
        s(t) shouldBe Some(u)
    }

    test("ior applies second strategy if first strategy succeeds") {
        val t = Add(Num(1), Num(2))
        val u = Add(Num(9), Num(8))
        val d = rule[Add] {
            case Add(l, r) => Add(Num(8), Num(9))
        }
        val e = rule[Add] {
            case Add(l, r) => Add(r, l)
        }
        val s = ior(d, e)
        s(t) shouldBe Some(u)
    }

    test("or applies second strategy and restores term if first strategy fails") {
        val t = Add(Num(1), Num(2))
        val d = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val e = rule[Add] {
            case Add(l, r) => Add(r, l)
        }
        val s = or(d, e)
        s(t) should beSomeOf(t)
    }

    test("or applies second strategy and restores term if first strategy succeeds") {
        val t = Add(Num(1), Num(2))
        val d = rule[Add] {
            case Add(l, r) => Add(Num(8), Num(9))
        }
        val e = rule[Add] {
            case Add(l, r) => Add(r, l)
        }
        val s = or(d, e)
        s(t) should beSomeOf(t)
    }

    test("and fails if the first strategy fails") {
        val t = Add(Num(1), Num(2))
        val d = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val e = rule[Add] {
            case Add(l, r) => Add(r, l)
        }
        val s = and(d, e)
        s(t) should beFailure
    }

    test("and fails if the first strategy succeeds but the second strategy fails") {
        val t = Add(Num(1), Num(2))
        val d = rule[Add] {
            case Add(l, r) => Add(r, l)
        }
        val e = rule[Num] {
            case Num(i) => Num(i + 1)
        }
        val s = and(d, e)
        s(t) should beFailure
    }

    test("and succeeds and restores term if both strategies succeed") {
        val t = Add(Num(1), Num(2))
        val d = rule[Add] {
            case Add(l, r) => Add(Num(8), Num(9))
        }
        val e = rule[Add] {
            case Add(l, r) => Add(r, l)
        }
        val s = and(d, e)
        s(t) should beSomeOf(t)
    }

    {
        val t = Mul(Add(Add(Num(1), Num(2)), Num(3)), Sub(Num(4), Num(5)))
        val u = Mul(Add(Add(Num(12), Num(13)), Num(14)), Sub(Num(16), Num(17)))

        test("everywhere traverses in expected order") {
            var l = Vector[Double]()
            var count = 9
            val r = rule[Any] {
                case Num(i) =>
                    l = l :+ i
                    Num(count)
                case n =>
                    count = count + 1
                    n
            }
            val s = everywhere(r)
            s(t) shouldBe Some(u)
            l shouldBe Vector(1, 2, 3, 4, 5)
        }

        test("everywheretd traverses in expected order") {
            var l = Vector[Double]()
            var count = 9
            val r = rule[Any] {
                case Num(i) =>
                    l = l :+ i
                    Num(count)
                case n =>
                    count = count + 1
                    n
            }
            val s = everywheretd(r)
            s(t) shouldBe Some(u)
            l shouldBe Vector(1, 2, 3, 4, 5)
        }

        test("everywherebu traverses in expected order") {
            var l = Vector[Double]()
            var count = 9
            val r = rule[Any] {
                case Num(i) =>
                    l = l :+ i
                    Num(count)
                case n =>
                    count = count + 1
                    n
            }
            val s = everywheretd(r)
            s(t) shouldBe Some(u)
            l shouldBe Vector(1, 2, 3, 4, 5)
        }
    }

    // Compilation tests

    test("rule that takes a basic type and returns the wrong basic type doesn't compile") {
        "rule[Int] { case i : Int => 3.5 }" shouldNot typeCheck
    }

    test("rule that takes a reference type and returns the wrong reference type doesn't compile") {
        "rule[Num] { case Num (i) => Var (\"i\") }" shouldNot typeCheck
    }

    test("rule that takes a reference type and returns a basic type doesn't compile") {
        "rule[Num] { case Num (i) => i }" shouldNot typeCheck
    }

    test("rule that takes a basic type and returns a reference type doesn't compile") {
        "rule[Int] { case i => Num (i) }" shouldNot typeCheck
    }

    test("strategy that takes a basic type and returns the wrong basic type doesn't compile") {
        "strategy[Int] { case i : Int => Some (3.5) }" shouldNot typeCheck
    }

    test("strategy that takes a reference type and returns the wrong reference type doesn't compile") {
        "strategy[Num] { case Num (i) => Some (Var (\"i\")) }" shouldNot typeCheck
    }

    test("strategy that takes a reference type and returns a basic type doesn't compile") {
        "strategy[Num] { case Num (i) => Some (i) }" shouldNot typeCheck
    }

    test("strategy that takes a basic type and returns a reference type doesn't compile") {
        "strategy[Int] { case i => Some (Num (i)) }" shouldNot typeCheck
    }

    // AnyVals

    {
        val s =
            everywheretd(rule[Any] {
                case AnyValClass(i) => AnyValClass(i + 1)
            })

        test("a structure containing just AnyVals rewrites correctly", FocusTest) {
            s(AnyValContainer(AnyValClass(1), AnyValClass(2))) shouldBe Some(AnyValContainer(AnyValClass(2), AnyValClass(3)))
        }

        test("a structure containing a mixture of AnyVals and other values rewrites correctly", FocusTest) {
            s(MixedValContainer(AnyValClass(1), 2, ProdClass(3))) shouldBe Some(MixedValContainer(AnyValClass(2), 2, ProdClass(3)))
        }
    }

}
