/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
import org.kiama.util.Tests
import org.scalatest.prop.Checkers

/**
 * Rewriting tests.
 */
class RewriterTests extends Tests with Checkers with Generator {

    import org.kiama.example.imperative.AST._

    /**
     * The rewriter which is being tested.
     */
    val rewriter : Rewriter = Rewriter

    import rewriter.{fail => rwfail, test => rwtest, _}

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
        check ((t : Stmt) => same (Some (t), issubterm (t, t)))
        check ((t : Exp) => same (Some (t), issubterm (t, t)))
    }

    test ("issubterm: random descendants are subterms") {
        val random = new scala.util.Random

        /**
         * Pick a random Term child of t, returning t if there are no
         * children or there are children but none of them are Terms.
         */
        def pickchild (t : Product) : Any = {
            val children = for (i <- 0 until t.productArity) yield t.productElement (i)
            if (children.length == 0)
                // No children, just use t itself
                t
            else {
                val termnum = random.nextInt (children.length)
                children (termnum)
            }
        }

        /**
         * Pick a random descendant of t (including possibly t).
         */
        def pickdesc (t : Any) : Any =
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

        check ((t : Stmt) => same (Some (t), issubterm (pickdesc (t), t)))
        check ((t : Exp) => same (Some (t), issubterm (pickdesc (t), t)))
    }

    {
        val t = Add (Num (1), Num (2))

        test ("issubterm: selected subterms - fail") {
            expectResult (None) (issubterm (Num (42), t))
        }

        test ("issubterm: selected subterms - succeed sub") {
            expectsame (Some (t)) (issubterm (Num (1), t))
        }

        test ("issubterm: selected subterms - succeed self") {
            expectsame (Some (t)) (issubterm (t, t))
        }

        test ("issubterm: selected proper subterms - fail") {
            expectResult (None) (ispropersubterm (Num (42), t))
        }

        test ("issubterm: selected proper subterms - succeed sub") {
            expectsame (Some (t)) (ispropersubterm (Num (1), t))
        }

        test ("issubterm: selected proper subterms - fail self") {
            expectResult (None) (ispropersubterm (t, t))
        }

        test ("issuperterm: selected superterms - fail") {
            expectResult (None) (issuperterm (t, Num (42)))
        }

        test ("issuperterm: selected superterms - succeed sub") {
            expectsame (Some (t)) (issuperterm (t, Num (1)))
        }

        test ("issuperterm: selected superterms - succeed self") {
            expectsame (Some (t)) (issuperterm (t, t))
        }

        test ("issuperterm: selected proper superterms - fail") {
            expectResult (None) (ispropersuperterm (t, Num (42)))
        }

        test ("issuperterm: selected proper superterms - succeed sub") {
            expectsame (Some (t)) (ispropersuperterm (t, Num (1)))
        }

        test ("issuperterm: selected proper superterms - fail self") {
            expectResult (None) (ispropersuperterm (t, t))
        }
    }

    test ("strategies that have no effect: identity") {
        check ((t : Stmt) => same (Some (t), id (t)))
        check ((t : Exp) => same (Some (t), id (t)))
    }

    test ("strategies that have no effect: some terms to themselves") {
        val noopstmt = everywherebu (rule { case Asgn (v, e) => Asgn (v, e) })
        check ((t : Stmt) => Some (t) == noopstmt (t))
        check ((t : Exp) => Some (t) == noopstmt (t))

        val noopexp = everywherebu (rule { case Num (i) => Num (i) })
        check ((t : Stmt) => Some (t) == noopexp (t))
        check ((t : Exp) => Some (t) == noopexp (t))
    }

    test ("strategies that fail immediately") {
        check ((t : Stmt) => rwfail (t) == None)
        check ((t : Exp) => rwfail (t) == None)
    }

    test ("where: failure") {
        check ((t : Exp) => where (rwfail) (t) == None)
    }

    test ("where: identity") {
        check ((t : Exp) => same (Some (t), where (id) (t)))
    }

    test ("where restores the original term after succcess") {
        val r = rule { case Num (i) => Num (i + 1) }
        val s = where (r)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("test: failure") {
        check ((t : Exp) => rwtest (rwfail) (t) == None)
    }

    test ("test: identity") {
        check ((t : Exp) => same (Some (t), rwtest (id) (t)))
    }

    test ("test restores the original term after succcess") {
        val r = rule { case Num (i) => Num (i + 1) }
        val s = rwtest (r)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("leaf detection") {
        check ((t : Exp) =>
            same (if (t.productArity == 0) Some (t) else None, isleaf (t)))
    }

    test ("innernode detection") {
        check ((t : Exp) =>
            same (if (t.productArity == 0) None else Some (t), isinnernode (t)))
    }

    test ("terms as strategies") {
        check ((t : Stmt, u : Exp) => same (Some (t), (build (t)) (u)))
        check ((t : Exp, u : Exp) => same (Some (t), (build (t)) (u)))
        check ((t : Stmt, u : Stmt) => same (Some (t), (build (t)) (u)))
        check ((t : Exp, u : Stmt) => same (Some (t), (build (t)) (u)))
    }

    test ("options as strategies") {
        check ((t : Stmt, u : Exp) => same (Some (t), option (Some (t)) (u)))
        check ((u : Exp) => same (None, option (None) (u)))
    }

    test ("term combinator") {
        check ((t : Stmt) => (term (t)) (t) == Some (t))
        check ((t : Exp) => (term (t)) (t) == Some (t))

        val t = Add (Num (1), Num (2))
        expectResult (None) (term (Num (1)) (t))
        expectResult (None) (term (Num (42)) (t))
    }

    {
        val e1 = Mul (Num (2), Num (3))
        val e2 = Add (Num (2), Num (3))

        test ("conditional choice operator: identity") {
            expectResult (Some (Num (1))) ((id < build (Num (1)) + build (Num (2))) (e1))
        }

        test ("conditional choice operator: failure") {
            expectResult (Some (Num (2))) ((rwfail < build (Num (1)) + build (Num (2))) (e1))
        }

        test ("conditional choice operator: condition for just success or failure") {
            val ismulbytwo = rule { case t @ Mul (Num (2), _) => t }
            val multoadd = rule { case Mul (Num (2), x) => Add (x, x) }
            val error : Strategy = build (Num (99))
            val trans1 = ismulbytwo < multoadd + error
            expectResult (Some (Add (Num (3), Num (3)))) ((trans1) (e1))
            expectResult (Some (Num (99))) ((trans1) (e2))
        }

        test ("conditional choice operator: condition that transforms object") {
            val mulbytwotoadd = rule { case t @ Mul (Num (2), x) => Add (x, x) }
            val add = rule { case Add (_, _) => Num (42) }
            val trans2 = mulbytwotoadd < add + id
            expectResult (Some (Num (42))) ((trans2) (e1))
            expectResult (Some (Add (Num (2), Num (3)))) ((trans2) (e2))
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
        expectResult (Some (Mul (Num (3), (Num (5))))) ((passtd) (e1))
        expectResult (Some (Add (Num (4), (Num (5))))) ((passtd) (e2))
    }

    {
        val e = Mul (Num (1), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))
        val ee = Mul (Num (1), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))

        test ("a bottomup traversal applying identity returns the same term") {
            expectsame (Some (e)) ((bottomup (id)) (e))
        }

        test ("a bottomup traversal applying identity doesn't returns term with same value") {
            expectnotsame (Some (ee)) ((bottomup (id)) (e))
        }

        test ("counting all terms using count") {
            val countall = count { case _ => 1 }
            expectResult (11) (countall (e))
        }

        test ("counting all terms using queryf") {
            var count = 0
            val countall = everywhere (queryf (_ => count = count + 1))
            expectsame (Some (e)) (countall (e))
            expectResult (11) (count)
        }

        test ("counting all terms using a para") {
            val countfold =
                para[Int] {
                    case (t, cs) => 1 + cs.sum
                }
            expectResult (11) (countfold (e))
        }

        test ("counting all Num terms twice") {
            val countnum = count { case Num (_) => 2 }
            expectResult (4) (countnum (e))
        }

        test ("counting all Div terms") {
            val countdiv = count { case Div (_, _) => 1 }
            expectResult (0) (countdiv (e))
        }

        test ("counting all binary operator terms, with Muls twice") {
            val countbin = count {
                case Add (_, _) => 1
                case Sub (_, _) => 1
                case Mul (_, _) => 2
                case Div (_, _) => 1
            }
            expectResult (4) (countbin (e))
        }

        {
            val l1 = List (Num (1), Num (2), Num (3))
            val r1 = List (Num (2), Num (3), Num (4))
            val l2 = List (Num (1), Var ("i"), Num (3))
            val l3 = List (Num (1))

            test ("map fail over a nil list gives nil") {
                expectResult (Some (Nil)) (map (rwfail) (Nil))
            }

            test ("map fail over a non-nil list fails") {
                expectResult (None) (map (rwfail) (l1))
            }

            test ("map id over a nil list gives nil") {
                expectResult (Some (Nil)) (map (id) (Nil))
            }

            test ("map id over a non-nil list gives that list") {
                expectsame (Some (l1)) (map (id) (l1))
            }

            {
                val inc = rule { case d : Double => d + 1 }

                test ("map double inc over a nil list gives nil") {
                    expectResult (Some (Nil)) (map (inc) (Nil))
                }

                test ("map double inc over a non-nil Num list fails") {
                    expectResult (None) (map (inc) (l1))
                }
            }

            {
                val isnum = rule { case n : Num => n }

                test ("map isnum over a nil list gives nil") {
                    expectResult (Some (Nil)) (map (isnum) (Nil))
                }

                test ("map isnum over a list with one num succeeds with same list") {
                    expectsame (Some (l3)) (map (isnum) (l3))
                }

                test ("map isnum over a list with more than one num succeeds with same list") {
                    expectsame (Some (l1)) (map (isnum) (l1))
                }

                test ("map isnum over a list with non-num fails") {
                    expectResult (None) (map (isnum) (l2))
                }

            }

            {
                val isnuminc = rule { case Num (i) => Num (i + 1) }

                test ("map isnuminc over a nil list gives nil") {
                    expectResult (Some (Nil)) (map (isnuminc) (Nil))
                }

                test ("map isnuminc over a list with only nums succeeds with incremented list") {
                    expectResult (Some (r1)) (map (isnuminc) (l1))
                }

                test ("map isnuminc over a list with non-num fails") {
                    expectResult (None) (map (isnuminc) (l2))
                }

            }
        }

        {
            val r = Mul (Num (2), Add (Sub (Var ("hello"), Num (3)), Var ("harold")))
            val s = Mul (Num (2), Add (Sub (Var ("hello"), Num (2)), Var ("harold")))

            val double = rule { case d : Double => d + 1 }

            test ("rewriting leaf types: increment doubles - all, topdown") {
                expectResult (Some (r)) ((alltd (double)) (e))
            }

            test ("rewriting leaf types: increment doubles - all, bottomup, same") {
                expectsame (Some (e)) ((allbu (double)) (e))
            }

            test ("rewriting leaf types: increment doubles - all, bottomup, not same") {
                expectnotsame (Some (ee)) ((allbu (double)) (e))
            }

            test ("rewriting leaf types: increment doubles - some, topdown") {
                expectResult (Some (r)) ((sometd (double)) (e))
            }

            test ("rewriting leaf types: increment doubles - some, bottomup") {
                expectResult (Some (r)) ((somebu (double)) (e))
            }

            test ("rewriting leaf types: increment doubles - one, topdown") {
                expectResult (Some (s)) ((oncetd (double)) (e))
            }

            test ("rewriting leaf types: increment doubles - one, bottomup") {
                expectResult (Some (s)) ((oncebu (double)) (e))
            }
        }

        {
            val r = Mul (Num (1), Add (Sub (Var ("olleh"), Num (2)), Var ("dlorah")))
            val s = Mul (Num (1), Add (Sub (Var ("olleh"), Num (2)), Var ("harold")))

            val rev = rule { case s : String => s.reverse }

            test ("rewriting leaf types: reverse identifiers - all, topdown") {
                expectResult (Some (r)) ((alltd (rev)) (e))
            }

            test ("rewriting leaf types: reverse identifiers - all, bottomup, same") {
                expectsame (Some (e)) ((allbu (rev)) (e))
            }

            test ("rewriting leaf types: reverse identifiers - all, bottomup, not same") {
                expectnotsame (Some (ee)) ((allbu (rev)) (e))
            }

            test ("rewriting leaf types: reverse identifiers - some, topdown") {
                expectResult (Some (r)) ((sometd (rev)) (e))
            }

            test ("rewriting leaf types: reverse identifiers - some, bottomup") {
                expectResult (Some (r)) ((somebu (rev)) (e))
            }

            test ("rewriting leaf types: reverse identifiers - one, topdown") {
                expectResult (Some (s)) ((oncetd (rev)) (e))
            }

            test ("rewriting leaf types: reverse identifiers - one, bottomup") {
                expectResult (Some (s)) ((oncebu (rev)) (e))
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

            test ("rewriting leaf types: increment even doubles and reverse idn - all, topdown") {
                expectResult (Some (r)) ((alltd (evendoubleincrev)) (e))
            }

            test ("rewriting leaf types: increment even doubles and reverse idn - all, bottomup, same") {
                expectsame (Some (e)) ((allbu (evendoubleincrev)) (e))
            }

            test ("rewriting leaf types: increment even doubles and reverse idn - all, bottomup, not same") {
                expectnotsame (Some (ee)) ((allbu (evendoubleincrev)) (e))
            }

            test ("rewriting leaf types: increment even doubles and reverse idn - some, topdown") {
                expectResult (Some (r)) ((sometd (evendoubleincrev)) (e))
            }

            test ("rewriting leaf types: increment even doubles and reverse idn - some, bottomup") {
                expectResult (Some (r)) ((somebu (evendoubleincrev)) (e))
            }

            test ("rewriting leaf types: increment even doubles and reverse idn - one, topdown") {
                expectResult (Some (s)) ((oncetd (evendoubleincrev)) (e))
            }

            test ("rewriting leaf types: increment even doubles and reverse idn - one, bottomup") {
                expectResult (Some (s)) ((oncebu (evendoubleincrev)) (e))
            }
        }
    }

    test ("rewrite to increment an integer") {
        val inc = rule { case i : Int => i + 1 }
        expectResult (Some (4)) ((inc) (3))
    }

    test ("rewrite to a constant value") {
        val const = rulef (_ => 88)
        expectResult (Some (88)) ((const) (3))
    }

    test ("rewrite failing to increment an integer with a double increment") {
        val inc = rule { case d : Double => d + 1 }
        expectResult (None) ((inc) (3))
    }

    {
        val incall = alltd (rule { case i : Int => i + 1 })
        val incfirst = oncetd (rule { case i : Int => i + 1 })
        val incodd = sometd (rule { case i : Int if i % 2 != 0 => i + 1 })

        test ("rewrite list: increment all numbers - non-empty") {
            expectResult (Some (List (2, 3, 4))) ((incall) (List (1, 2, 3)))
        }

        test ("rewrite list: increment all numbers - empty") {
            expectResult (Some (Nil)) ((incall) (Nil))
        }

        test ("rewrite list: increment first number - non-empty") {
            expectResult (Some (List (2, 2, 3))) ((incfirst) (List (1, 2, 3)))
        }

        test ("rewrite list: increment first number - empty") {
            expectResult (None) ((incfirst) (Nil))
        }

        test ("rewrite list: increment odd numbers - succeed") {
            expectResult (Some (List (2, 2, 4))) ((incodd) (List (1, 2, 3)))
        }

        test ("rewrite list: increment odd numbers - fail") {
            expectResult (None) ((incodd) (List (2, 4, 6)))
        }

        val l = List (List (1, 2), List (3), List (4, 5, 6))

        test ("rewrite list: nested increment all numbers") {
            expectResult (Some (List (List (2, 3), List (4), List (5, 6, 7)))) ((incall) (l))
        }

        test ("rewrite list: nested increment first number") {
            expectResult (Some (List (List (2, 2), List (3), List (4, 5, 6)))) ((incfirst) (l))
        }

        test ("rewrite list: nested increment odd numbers - succeed") {
            expectResult (Some (List (List (2, 2), List (4), List (4, 6, 6)))) ((incodd) (l))
        }

        test ("rewrite list: nested increment odd numbers - fail") {
            expectResult (None) ((incodd) (List (List (2, 2), List (4), List (4, 6, 6))))
        }
    }

    test ("same comparison of equal references yields true xxxx") {
        class Num (i : Int)
        val r = new Num (42)
        expectResult (true) (same (r, r))
    }

    test ("same comparison of unequalt references yields false") {
        class Num (i : Int)
        val r1 = new Num (42)
        val r2 = new Num (42)
        expectResult (false) (same (r1, r2))
    }

    test ("same comparison of equal non-references yields true") {
        expectResult (true) (same (42, 42))
    }


    test ("same comparison of unequalt non-references yields false") {
        expectResult (false) (same (42, 43))
    }

    /**
     * The kind of comparison that is expected to be true for a test.  Equal
     * means use ==.  Same means the result must be the same reference or, if
     * the values are not references, use ==.  NotSame is the opposite of Same.
     */
    abstract class Expecting
    case object Equal extends Expecting
    case object Same extends Expecting
    case object NotSame extends Expecting

    def travtest (basemsg : String, testmsg : String,
                  eval : Option[Any], result : Option[Any],
                  expecting : Expecting = Equal) {
        val msg = basemsg + " - " + testmsg + ", " + expecting
        test (msg) {
            expecting match {
                case Equal   => expectResult (result) (eval)
                case Same    => expectsame (result) (eval)
                case NotSame => expectnotsame (result) (eval)
            }
        }
    }

    {
        val l = List (Sub (Num (2), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        val ll = List (Sub (Num (2), Var ("one")), Add (Num (4), Num (5)), Var ("two"))
        val r = List (Sub (Num (0), Var ("one")), Add (Num (0), Num (0)), Var ("two"))
        val s = List (Sub (Num (0), Var ("one")), Add (Num (4), Num (5)), Var ("two"))

        val strat = rule { case _ : Double => 0 }
        val basemsg = "rewrite list: doubles to zero in non-primitive list"

        travtest (basemsg, "all, topdown", alltd (strat) (l), Some (r))
        travtest (basemsg, "all, bottomup", allbu (strat) (l), Some (l), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (l), Some (ll), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (l), Some (r))
        travtest (basemsg, "some, bottomup", somebu (strat) (l), Some (r))
        travtest (basemsg, "one, topdown", oncetd (strat) (l), Some (s))
        travtest (basemsg, "one, bottomup", oncebu (strat) (l), Some (s))
    }

    {
        val v = Set (1, 5, 8, 9)
        val vv = Set (1, 5, 8, 9)

        val strat = rule { case i : Int => i }
        val basemsg = "rewrite set: no change"

        travtest (basemsg, "all, topdown", alltd (strat) (v), Some (v), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (v), Some (v), Same)
        travtest (basemsg, "some, topdown", sometd (strat) (v), Some (v), Same)
        travtest (basemsg, "some, bottomup", somebu (strat) (v), Some (v), Same)
        travtest (basemsg, "one, topdown", oncetd (strat) (v), Some (v), Same)
        travtest (basemsg, "one, bottomup", oncebu (strat) (v), Some (v), Same)

        travtest (basemsg, "all, topdown", alltd (strat) (v), Some (vv), NotSame)
        travtest (basemsg, "all, bottomup", allbu (strat) (v), Some (vv), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (v), Some (vv), NotSame)
        travtest (basemsg, "some, bottomup", somebu (strat) (v), Some (vv), NotSame)
        travtest (basemsg, "one, topdown", oncetd (strat) (v), Some (vv), NotSame)
        travtest (basemsg, "one, bottomup", oncebu (strat) (v), Some (vv), NotSame)
    }

    {
        val r = Set (1, 5, 8, 9)
        val rr = Set (1, 5, 8, 9)
        val s = Set (2, 10, 16, 18)
        val t = Set (2, 5, 8, 9)

        val strat = rule { case i : Int => i * 2 }
        val basemsg = "rewrite set: double value"

        travtest (basemsg, "all, topdown", alltd (strat) (r), Some (s))
        travtest (basemsg, "all, bottomup", allbu (strat) (r), Some (r), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (r), Some (rr), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (r), Some (s))
        travtest (basemsg, "some, bottomup", somebu (strat) (r), Some (s))
        travtest (basemsg, "one, topdown", oncetd (strat) (r), Some (t))
        travtest (basemsg, "one, bottomup", oncebu (strat) (r), Some (t))
    }

    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)

        val strat = rule { case s : String => s }
        val basemsg = "rewrite map: no change"

        travtest (basemsg, "all, topdown", alltd (strat) (m), Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (m), Same)
        travtest (basemsg, "some, topdown", sometd (strat) (m), Some (m), Same)
        travtest (basemsg, "some, bottomup", somebu (strat) (m), Some (m), Same)
        travtest (basemsg, "one, topdown", oncetd (strat) (m), Some (m), Same)
        travtest (basemsg, "one, bottomup", oncebu (strat) (m), Some (m), Same)

        travtest (basemsg, "all, topdown", alltd (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (m), Some (mm), NotSame)
        travtest (basemsg,"some, bottomup", somebu (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "one, topdown", oncetd (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "one, bottomup", oncebu (strat) (m), Some (mm), NotSame)
    }

    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("eno" -> 1, "owt" -> 2, "eerht" -> 3)
        val s = Map ("eno" -> 1, "two" -> 2, "three" -> 3)

        val strat = rule { case s : String => s.reverse }
        val basemsg = "rewrite map: reverse keys"

        travtest (basemsg, "all, topdown", alltd (strat) (m), Some (r))
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (m), Some (r))
        travtest (basemsg, "some, bottomup", somebu (strat) (m), Some (r))
        travtest (basemsg, "one, topdown", oncetd (strat) (m), Some (s))
        travtest (basemsg, "one, bottomup", oncebu (strat) (m), Some (s))
    }

    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("one" -> 2, "two" -> 3, "three" -> 4)
        val s = Map ("one" -> 2, "two" -> 2, "three" -> 3)

        val strat = rule { case i : Int => i + 1 }
        val basemsg = "rewrite map: increment values"

        travtest (basemsg, "all, topdown", alltd (strat) (m), Some (r))
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (m), Some (r))
        travtest (basemsg, "some, bottomup", somebu (strat) (m), Some (r))
        travtest (basemsg, "one, topdown", oncetd (strat) (m), Some (s))
        travtest (basemsg, "one, bottomup", oncebu (strat) (m), Some (s))
    }

    {
        val m = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val mm = Map ("one" -> 1, "two" -> 2, "three" -> 3)
        val r = Map ("eno" -> 2, "owt" -> 3, "eerht" -> 4)
        val s = Map ("eno" -> 1, "two" -> 2, "three" -> 3)

        val basemsg = "rewrite map: reverse keys and increment values"
        val strat = rule {
                        case s : String => s.reverse
                        case i : Int    => i + 1
                    }

        travtest (basemsg, "all, topdown", alltd (strat) (m), Some (r))
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (m), Some (r))
        travtest (basemsg, "some, bottomup", somebu (strat) (m), Some (r))
        travtest (basemsg, "one, topdown", oncetd (strat) (m), Some (s))
        travtest (basemsg, "one, bottomup", oncebu (strat) (m), Some (s))
    }

    {
        val m = Map (1 -> 2, 3 -> 4, 5 -> 6)
        val mm = Map (1 -> 2, 3 -> 4, 5 -> 6)
        val r = Map (2 -> 4, 4 -> 8, 6 -> 12)
        val s = Map (2 -> 4, 3 -> 4, 5 -> 6)

        val basemsg = "rewrite map: increment key and double value"
        val strat = rule { case (k : Int, v : Int) => (k + 1, v * 2) }

        travtest (basemsg, "all, topdown", alltd (strat) (m), Some (r))
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (m), Same)
        travtest (basemsg, "all, bottomup", allbu (strat) (m), Some (mm), NotSame)
        travtest (basemsg, "some, topdown", sometd (strat) (m), Some (r))
        travtest (basemsg, "some, bottomup", somebu (strat) (m), Some (r))
        travtest (basemsg, "one, topdown", oncetd (strat) (m), Some (s))
        travtest (basemsg, "one, bottomup", oncebu (strat) (m), Some (s))
    }

    {
        // Maps from sets to their sizes, on init size is always zero
        val m1 = Map (Set (1, 3) -> 0, Set (2, 4, 6) -> 0)
        val m2 = Map (Set (12, 16) -> 0, Set (23) -> 0)

        // List of the maps
        val l = List (m1, m2)
        val ll = List (Map (Set (1, 3) -> 0, Set (2, 4, 6) -> 0),
                       Map (Set (12, 16) -> 0, Set (23) -> 0))

        {
            val r = List (Map (Set (2, 4) -> 1, Set (3, 5, 7) -> 1),
                          Map (Set (13, 17) -> 1, Set (24) -> 1))
            val s = List (Map (Set (2, 3) -> 0, Set (2, 4, 6) -> 0),
                          Map (Set (12, 16) -> 0, Set (23) -> 0))

            val basemsg = "rewrite set: heterogeneous collection: inc integers"
            val strat = rule { case i : Int => i + 1 }

            travtest (basemsg, "all, topdown", alltd (strat) (l), Some (r))
            travtest (basemsg, "all, bottomup", allbu (strat) (l), Some (l), Same)
            travtest (basemsg, "all, bottomup", allbu (strat) (l), Some (ll), NotSame)
            travtest (basemsg, "some, topdown", sometd (strat) (l), Some (r))
            travtest (basemsg, "some, bottomup", somebu (strat) (l), Some (r))
            travtest (basemsg, "one, topdown", oncetd (strat) (l), Some (s))
            travtest (basemsg, "one, bottomup", oncebu (strat) (l), Some (s))
        }

        {
            val r = List (Map (Set (1, 3) -> 2, Set (2, 4, 6) -> 3),
                          Map (Set (12, 16) -> 2, Set (23) -> 1))
            val s = List (Map (Set (1, 3) -> 2, Set (2, 4, 6) -> 0),
                          Map (Set (12, 16) -> 0, Set (23) -> 0))

            val basemsg = "rewrite set: heterogeneous collection: set to size"
            val strat = rule { case (s : Set[_], _) => (s, s.size) }

            travtest (basemsg, "all, topdown", alltd (strat) (l), Some (r))
            travtest (basemsg, "all, bottomup", allbu (strat) (l), Some (l), Same)
            travtest (basemsg, "all, bottomup", allbu (strat) (l), Some (ll), NotSame)
            travtest (basemsg, "some, topdown", sometd (strat) (l), Some (r))
            travtest (basemsg, "some, bottomup", somebu (strat) (l), Some (r))
            travtest (basemsg, "one, topdown", oncetd (strat) (l), Some (s))
            travtest (basemsg, "one, bottomup", oncebu (strat) (l), Some (s))
        }
    }

    {
        val l = Add (Num (1), Num (2))
        val r = Add (Num (3), Num (4))
        val t = Sub (l, r)

        val incnum = rule { case Num (i) => Num (i + 1) }
        val inczerothchild = child (0, incnum)
        val incfirstchild = child (1, incnum)
        val incsecondchild = child (2, incnum)
        val incthirdchild = child (3, incnum)
        val incallsecondchild = alltd (incsecondchild)

        test ("rewrite by child index: inc zeroth child - fail") {
            expectResult (None) (inczerothchild (Add (Num (2), Num (3))))
        }

        test ("rewrite by child index: inc first child - fail") {
            expectResult (None) (incfirstchild (Num (2)))
        }

        test ("rewrite by child index: inc first child - succeed, one child, one level") {
            expectResult (Some (Neg (Num (3)))) (incfirstchild (Neg (Num (2))))
        }

        test ("rewrite by child index: inc first child - succeed, two children, one level") {
            expectResult (Some (Add (Num (3), Num (3)))) (incfirstchild (Add (Num (2), Num (3))))
        }

        test ("rewrite by child index: inc second child - fail") {
            expectResult (None) (incsecondchild (Num (2)))
        }

        test ("rewrite by child index: inc second child - succeed, one level") {
            expectResult (Some (Add (Num (2), Num (4)))) (incsecondchild (Add (Num (2), Num (3))))
        }

        test ("rewrite by child index: inc third child - fail, one level") {
            expectResult (None) (incthirdchild (Add (Num (2), Num (3))))
        }

        test ("rewrite by child index: inc second child - succeed, multi-level") {
            expectResult (Some (Sub (Add (Num (2), Num (4)), Mul (Num (4), Num (6))))) (
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

        test ("rewrite linkedlist by child index: inc zeroth child - fail, empty") {
            expectResult (None) (inczerothchild (l1))
        }

        test ("rewrite linkedlist by child index: inc first child - fail, empty") {
            expectResult (None) (incfirstchild (l1))
        }

        test ("rewrite linkedlist by child index: inc first child - succeed, singleton") {
            expectResult (Some (LinkedList (2))) (incfirstchild (l2))
        }

        test ("rewrite linkedlist by child index: inc second child - fail, singleton") {
            expectResult (None) (incsecondchild (l2))
        }

        test ("rewrite linkedlist by child index: inc zeroth child - fail, multiple") {
            expectResult (None) (inczerothchild (l3))
        }

        test ("rewrite linkedlist by child index: inc first child - succeed, multiple") {
            expectResult (Some (LinkedList (2, 2, 3, 4))) (incfirstchild (l3))
        }

        test ("rewrite linkedlist by child index: inc second child - succeed, one level") {
            expectResult (Some (LinkedList (1, 3, 3, 4))) (incsecondchild (l3))
        }

        test ("rewrite linkedlist by child index: inc second child - succeed, multi-level") {
            expectResult (Some (LinkedList (LinkedList (1), LinkedList (3, 5, 5), LinkedList (6, 8)))) (
                incallsecondchild (LinkedList (LinkedList (1), LinkedList (3, 4, 5), LinkedList (6, 7)))
            )
        }
    }

    {
        // { i = 10; count = 0; while (i) { count = count + 1; i = 1 + i; } }
        val p =
            Seqn (List (
                Asgn (Var ("i"), Num (10)),
                Asgn (Var ("count"), Num (0)),
                While (Var ("i"),
                    Seqn (List (
                        Asgn (Var ("count"), Add (Var ("count"), Num (1))),
                        Asgn (Var ("i"), Add (Num (1), Var ("i"))))))))

        // { i = 0; count = 0; while (i) { count = bob + 1; i = 0 + i; } }
        val q =
            Seqn (List (
                Asgn (Var ("i"), Num (0)),
                Asgn (Var ("count"), Num (0)),
                While (Var ("i"),
                    Seqn (List (
                        Asgn (Var ("count"), Add (Var ("bob"), Num (1))),
                        Asgn (Var ("i"), Add (Num (0), Var ("i"))))))))

        val incint = rule { case i : Int => i + 1 }
        val clearlist = rule { case _ => Nil }
        val zeronumsbreakadds =
            alltd (Num (rule { case _ => 0}) +
                   Add (rule { case Var (_) => Var ("bob")}, id))

        test ("rewrite by congruence: top-level wrong congruence") {
            expectResult (None) (Num (incint) (p))
        }

        test ("rewrite by congruence: top-level correct congruence") {
            expectResult (Some (Seqn (Nil))) (Seqn (clearlist) (p))
        }

        test ("rewrite by congruence: multi-level") {
            expectResult (Some (q)) (zeronumsbreakadds (p))
        }
    }

    test ("debug strategy produces the expected message and result") {
        import org.kiama.util.StringEmitter
        val e = new StringEmitter
        val s = debug ("hello there: ", e)
        val t = Asgn (Var ("i"), Add (Num (1), Var ("i")))
        expectsame (Some (t)) (s (t))
        expectResult ("hello there: " + t + "\n") (e.result)
    }

    test ("log strategy produces the expected message and result on success") {
        import org.kiama.util.StringEmitter
        val e = new StringEmitter
        var r = rule { case Asgn (l, r) => Asgn (l, Num (42)) }
        val s = log (r, "test log ", e)
        val t = Asgn (Var ("i"), Add (Num (1), Var ("i")))
        val u = Asgn (Var ("i"), Num (42))
        expectResult (Some (u)) (s (t))
        expectResult ("test log " + t + " succeeded with " + u + "\n") (e.result)
    }

    test ("log strategy produces the expected message and result on failure") {
        import org.kiama.util.StringEmitter
        val e = new StringEmitter
        var r = rule { case Asgn (l, r) => Asgn (l, Num (42)) }
        val s = log (r, "test log ", e)
        val t = Add (Num (1), Var ("i"))
        expectResult (None) (s (t))
        expectResult ("test log " + t + " failed\n") (e.result)
    }

    test ("logfail strategy produces no message but the right result on success") {
        import org.kiama.util.StringEmitter
        val e = new StringEmitter
        var r = rule { case Asgn (l, r) => Asgn (l, Num (42)) }
        val s = logfail (r, "test log ", e)
        val t = Asgn (Var ("i"), Add (Num (1), Var ("i")))
        val u = Asgn (Var ("i"), Num (42))
        expectResult (Some (u)) (s (t))
        expectResult ("") (e.result)
    }

    test ("logfail strategy produces the expected message and result on failure") {
        import org.kiama.util.StringEmitter
        val e = new StringEmitter
        var r = rule { case Asgn (l, r) => Asgn (l, Num (42)) }
        val s = logfail (r, "test log ", e)
        val t = Add (Num (1), Var ("i"))
        expectResult (None) (s (t))
        expectResult ("test log " + t + " failed\n") (e.result)
    }

    test ("rewrite returns the original term when the strategy fails") {
        val t = Asgn (Var ("i"), Add (Num (1), Var ("i")))
        expectsame (Some (t)) (Some (rewrite (rwfail) (t)))
    }

    test ("rewrite returns the strategy result when the strategy succeeds") {
        val t = Asgn (Var ("i"), Add (Num (1), Var ("i")))
        val s = everywhere (rule { case Var (_) => Var ("hello") })
        expectResult (s (t)) (Some (rewrite (s) (t)))
    }

    test ("a memo strategy returns the previous result") {
        val t = Asgn (Var ("i"), Add (Num (1), Var ("i")))
        var count = 0
        val s = memo (everywhere (rule {
                    case Var (_) => count = count + 1;
                                    Var ("i" + count)
                }))
        val r = Some (Asgn (Var ("i1"), Add (Num (1), Var ("i2"))))
        expectResult (r) (s (t))
        expectResult (r) (s (t))
    }

    test ("an illegal dup throws an appropriate exception") {
        val t = Asgn (Var ("i"), Add (Num (1), Var ("i")))
        val s = everywhere (rule { case Var (_) => 42 })
        val i = intercept[RuntimeException] { s (t) }
        val base = "dup illegal arguments"
        val method = "public org.kiama.example.imperative.AST$Add"
        val argtype = "org.kiama.example.imperative.AST$Exp"
        val error = "(Num(1.0),42), expects 2"
        val msg = "%s: %s(%s,%s) %s".format (base, method, argtype, argtype, error)
        expectResult (msg) (i.getMessage)
    }

    test ("repeat on failure succeeds") {
        val s = repeat (rwfail)
        val t = Num (10)
        expectsame (Some (t)) (s (t))
    }

    test ("repeat of non-failure works") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val s = repeat (r)
        expectResult (Some (Num (10))) (s (Num (1)))
    }

    test ("repeat with a final strategy on failure applies the final strategy") {
        val f = rule {
                    case Num (10) => Num (20)
                }
        val s = repeat (rwfail, f)
        expectResult (Some (Num (20))) (s (Num (10)))
    }

    test ("repeat with a final strategy works") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val f = rule {
                    case Num (10) => Num (20)
                }
        val s = repeat (r, f)
        expectResult (Some (Num (20))) (s (Num (1)))
    }

    test ("repeat with a final failure fails") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val s = repeat (r, rwfail)
        expectResult (None) (s (Num (1)))
    }

    test ("repeat1 on failure fails") {
        val s = repeat1 (rwfail)
        expectResult (None) (s (Num (10)))
    }

    test ("repeat1 of non-failure works") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val s = repeat1 (r)
        expectResult (Some (Num (10))) (s (Num (1)))
    }

    test ("repeat1 with a final strategy on failure doesn't apply the final strategy") {
        val f = rule {
                    case Num (10) => Num (20)
                }
        val s = repeat1 (rwfail, f)
        expectResult (None) (s (Num (10)))
    }

    test ("repeat1 with a final strategy works") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val f = rule {
                    case Num (10) => Num (20)
                }
        val s = repeat1 (r, f)
        expectResult (Some (Num (20))) (s (Num (1)))
    }

    test ("repeat1 with a final failure fails") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val s = repeat1 (r, rwfail)
        expectResult (None) (s (Num (1)))
    }

    test ("zero repeat of failure is identity") {
        val s = repeat (rwfail, 0)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("non-zero repeat of failure fails") {
        val s = repeat (rwfail, 4)
        expectResult (None) (s (Num (1)))
    }

    test ("zero repeat of non-failure is identity") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val s = repeat (r, 0)
        val t = Num (1)
        expectResult (Some (t)) (s (t))
    }

    test ("non-zero repeat of non-failure is repeated correct number of times") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val s = repeat (r, 4)
        expectResult (Some (Num (5))) (s (Num (1)))
    }

    test ("repeatuntil on failure fails") {
        val f = rule {
                    case Num (10) => Num (20)
                }
        val s = repeatuntil (rwfail, f)
        expectResult (None) (s (Num (1)))
    }

    test ("repeatuntil on non-failure works") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i + 1)
                }
        val f = rule {
                    case Num (10) => Num (20)
                }
        val s = repeatuntil (r, f)
        expectResult (Some (Num (20))) (s (Num (1)))
    }

    test ("loop on failure is identity") {
        val f = rule {
                    case Num (1) => Num (2)
                }
        val s = loop (rwfail, f)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("loop on non-failure with initially false condition is identity") {
        val r = rule {
                    case Num (i) if i > 10 => Num (i)
                }
        val f = rule {
                    case Num (1) => Num (2)
                }
        val s = loop (r, f)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("loop on failure with initially true condition is identity") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i)
                }
        val s = loop (r, rwfail)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("loop on non-failure with initially true condition works") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i)
                }
        val f = rule {
                    case Num (i) => Num (i + 1)
                }
        val s = loop (r, f)
        expectResult (Some (Num (10))) (s (Num (1)))
    }

    test ("loopnot on succeess is identity") {
        val f = rule {
                    case Num (1) => Num (2)
                }
        val s = loopnot (id, f)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("loopnot on non-failure with initially true condition is identity") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i)
                }
        val f = rule {
                    case Num (1) => Num (2)
                }
        val s = loopnot (r, f)
        val t = Num (1)
        expectResult (Some (t)) (s (t))
    }

    test ("loopnot on failure with initially false condition fails") {
        val r = rule {
                    case Num (i) if i >= 10 => Num (i + 1)
                }
        val s = loopnot (r, rwfail)
        expectResult (None) (s (Num (1)))
    }

    test ("loopnot on non-failure with initially false condition works") {
        val r = rule {
                    case Num (i) if i >= 10 => Num (i)
                }
        val f = rule {
                    case Num (i) => Num (i + 1)
                }
        val s = loopnot (r, f)
        expectResult (Some (Num (10))) (s (Num (1)))
    }

    test ("doloop on failure applies once") {
        val f = rule {
                    case Num (i) => Num (i + 1)
                }
        val s = doloop (f, rwfail)
        expectResult (Some (Num (2))) (s (Num (1)))
    }

    test ("doloop on non-failure with initially false condition applies once") {
        val r = rule {
                    case Num (i) => Num (i + 1)
                }
        val f = rule {
                    case Num (i) if i >= 10 => Num (i)
                }
        val s = doloop (r, f)
        expectResult (Some (Num (2))) (s (Num (1)))
    }

    test ("doloop on failure with initially true condition is failure") {
        val f = rule {
                    case Num (i) if i < 10 => Num (i)
                }
        val s = doloop (rwfail, f)
        expectResult (None) (s (Num (1)))
    }

    test ("doloop on non-failure with initially true condition works") {
        val r = rule {
                    case Num (i) => Num (i + 1)
                }
        val f = rule {
                    case Num (i) if i < 10 => Num (i)
                }
        val s = doloop (r, f)
        expectResult (Some (Num (10))) (s (Num (1)))
    }

    test ("loopiter with failure init fails") {
        val r = rule {
                    case Num (i) if i < 10 => Num (i)
                }
        val f = rule {
                    case Num (1) => Num (2)
                }
        val s = loopiter (rwfail, r, f)
        expectResult (None) (s (Num (1)))
    }

    test ("loopiter with succeeding init and initially true condition works") {
        val i = rule {
                    case Num (100) => Num (1)
                }
        val r = rule {
                    case Num (i) if i < 10 => Num (i)
                }
        val f = rule {
                    case Num (1) => Num (2)
                }
        val s = loopiter (i, r, f)
        expectResult (Some (Num (1))) (s (Num (100)))
    }

    test ("loopiter with succeeding init and initially false condition works") {
        val i = rule {
                    case Num (100) => Num (1)
                }
        val r = rule {
                    case Num (i) if i >= 10 => Num (i)
                }
        val f = rule {
                    case Num (i) => Num (i + 1)
                }
        val s = loopiter (i, r, f)
        expectResult (Some (Num (10))) (s (Num (100)))
    }

    test ("counting loopiter is identity if there is nothing to count") {
        val r = (i : Int) =>
                    rule {
                        case Num (j) => Num (i + j)
                    }
        val s = loopiter (r, 10, 1)
        val t = Num (1)
        expectsame (Some (t)) (s (t))
    }

    test ("counting loopiter counts correctly") {
        var count = 0
        val r = (i : Int) =>
                    rule {
                        case Num (j) => count = count + i
                                        Num (j + 1)
                    }
        val s = loopiter (r, 1, 10)
        expectResult (Some (Num (11))) (s (Num (1)))
        expectResult (55) (count)
    }

    test ("breadthfirst traverses in correct order") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        var l : List[Double] = Nil
        val r = rule {
                    case n @ Num (i) => l = l :+ i
                                        n
                    case n           => n
                }
        val s = breadthfirst (r)
        expectsame (Some (t)) (s (t))
        expectResult (List (3, 1, 2, 4, 5)) (l)
    }

    test ("leaves with a failing leaf detector succeeds but doesn't collect anything") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        var sum = 0.0
        val r = rule {
                    case Num (i) => sum = sum + i
                }
        val s = leaves (r, rwfail)
        expectResult (Some (t)) (s (t))
        expectResult (0) (sum)
    }

    test ("leaves with a non-failing leaf detector succeeds and collects correctly") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        var sum = 0.0
        val r = rule {
                    case Num (i) => sum = sum + i
                                    Num (i)
                }
        val l = rule {
                    case Num (i) if (i % 2 != 1) => Num (i)
                }
        val s = leaves (r, l)
        expectResult (Some (t)) (s (t))
        expectResult (6) (sum)
    }

    test ("skipping leaves with a non-failing leaf detector succeeds and collects correctly") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        var sum = 0.0
        val r = rule {
                    case Num (i) => sum = sum + i
                                    Num (i)
                }
        val l = rule {
                    case Num (i) if (i % 2 != 0) => Num (i)
                }
        val x = (y : Strategy) =>
                    rule {
                        case n @ Sub (_, _) => n
                    }
        val s = leaves (r, l, x)
        expectResult (Some (t)) (s (t))
        expectResult (4) (sum)
    }

    {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Add (Var ("1.0"), Var ("2.0")), Var ("3.0")), Sub (Var ("4.0"), Var ("5.0")))

        test ("innermost visits the correct nodes in the correct order") {
            var l : List[Double] = Nil
            val r = rule {
                        case Num (i) => l = l :+ i
                                        Var (i.toString)
                    }
            val s = innermost (r)
            expectResult (Some (u)) (s (t))
            expectResult (List (1, 2, 3, 4, 5)) (l)
        }

        test ("innermost2 visits the correct node") {
            var l : List[Double] = Nil
            val r = rule {
                        case Num (i) => l = l :+ i
                                        Var (i.toString)
                    }
            val s = innermost2 (r)
            expectResult (Some (u)) (s (t))
            expectResult (List (1, 2, 3, 4, 5)) (l)
        }

    }

    test ("downup (one arg version) visits the correct frontier") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (1), Num (2))), Sub (Num (4), Num (5)))
        val d = rule {
                    case Add (l, r @ Num (3)) => Add (r, l)
                    case Sub (l, r)           => Sub (r, l)
                    case n                    => n
                }
        val s = downup (d)
        expectResult (Some (u)) (s (t))
    }

    test ("downup (two arg version) visits the correct frontier") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (1), Num (2))), Sub (Num (8), Num (9)))
        val d = rule {
                    case Add (l, r @ Num (3)) => Add (r, l)
                    case Sub (l, r)           => Sub (r, l)
                    case n                    => n
                }
        val e = rule {
                    case Sub (l, r)           => Sub (Num (8), Num (9))
                    case n                    => n
                }
        val s = downup (d, e)
        expectResult (Some (u)) (s (t))
    }

    test ("somedownup visits the correct frontier") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Add (Num (3), Num (4)), Num (3)), Sub (Num (2), Num (3)))
        val d = rule {
                    case Add (Num (l), Num (r)) => Add (Num (l + 1), Num (r + 1))
                    case Sub (Num (l), Num (r)) => Sub (Num (l - 1), Num (r - 1))
                    case n : Mul                => n
                }
        val s = somedownup (d)
        expectResult (Some (u)) (s (t))
    }

    test ("downupS (two arg version) visits the correct frontier") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (1), Num (2))), Sub (Num (4), Num (5)))
        val d = rule {
                    case Add (l, r @ Num (3)) => Add (r, l)
                    case Sub (l, r)           => Sub (r, l)
                    case n                    => n
                }
        def f (y : => Strategy) : Strategy =
            rule {
                case n @ Add (_, Num (3)) => n
            }
        val s = downupS (d, f)
        expectResult (Some (u)) (s (t))
    }

    test ("downupS (three arg version) visits the correct frontier") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (1), Num (2))), Sub (Num (8), Num (9)))
        val d = rule {
                    case Add (l, r @ Num (3)) => Add (r, l)
                    case Sub (l, r)           => Sub (r, l)
                    case n                    => n
                }
        val e = rule {
                    case Sub (l, r)           => Sub (Num (8), Num (9))
                    case n                    => n
                }
        def f (y : => Strategy) : Strategy =
            rule {
                case n @ Add (_, Num (3)) => n
            }
        val s = downupS (d, e, f _)
        expectResult (Some (u)) (s (t))
    }

    test ("alldownup2 visits the correct frontier") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Mul (Num (3), Add (Num (1), Num (2))), Sub (Num (5), Num (4)))
        val d = rule {
                    case Add (l, r @ Num (3)) => Add (r, l)
                    case Sub (l, r)           => Sub (r, l)
                }
        val e = rule {
                    case Add (l, r) => Mul (l, r)
                    case n          => n
                }
        val s = alldownup2 (d, e)
        expectResult (Some (u)) (s (t))
    }

    test ("topdownS stops at the right spots") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (1), Num (2))), Sub (Num (5), Num (4)))
        val d = rule {
                    case Add (l, r) => Add (r, l)
                    case Sub (l, r) => Sub (r, l)
                    case n          => n
                }
        def f (y : => Strategy) : Strategy =
            rule {
                case n @ Add (Num (3), _) => n
            }
        val s = topdownS (d, f)
        expectResult (Some (u)) (s (t))
    }

    test ("topdownS with no stopping doesn't stop") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (2), Num (1))), Sub (Num (5), Num (4)))
        val d = rule {
                    case Add (l, r) => Add (r, l)
                    case Sub (l, r) => Sub (r, l)
                    case n          => n
                }
        val s = topdownS (d, dontstop)
        expectResult (Some (u)) (s (t))
    }

    test ("bottomupS stops at the right spots") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (1), Num (2))), Sub (Num (5), Num (4)))
        val d = rule {
                    case Add (l, r) => Add (r, l)
                    case Sub (l, r) => Sub (r, l)
                    case n          => n
                }
        def f (y : => Strategy) : Strategy =
            rule {
                case n @ Add (_, Num (3)) => n
            }
        val s = bottomupS (d, f)
        expectResult (Some (u)) (s (t))
    }

    test ("bottomupS with no stopping doesn't stop") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (3), Add (Num (2), Num (1))), Sub (Num (5), Num (4)))
        val d = rule {
                    case Add (l, r) => Add (r, l)
                    case Sub (l, r) => Sub (r, l)
                    case n          => n
                }
        val s = bottomupS (d, dontstop)
        expectResult (Some (u)) (s (t))
    }

    test ("manybu applies the strategy in the right order and right number of times") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Add (Num (12), Num (11)), Num (10)), Sub (Num (4), Num (5)))
        var count = 13
        val d = rule {
                    case Num (i) if (count > 10) => count = count - 1
                                                    Num (count)
                }
        val s = manybu (d)
        expectResult (Some (u)) (s (t))
    }

    test ("manytd applies the strategy in the right order and right number of times") {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Num (11), Add (Num (2), Num (1))), Sub (Num (4), Num (5)))
        var count = 13
        val d = rule {
                    case Num (i) if (count > 10) =>
                        count = count - 1
                        Num (count)
                    case Add (l, r) if (count > 10) =>
                        count = count - 1
                        Add (r, l)
                }
        val s = manytd (d)
        expectResult (Some (u)) (s (t))
    }

    test ("alltdfold can be used to evaluate an expression") {
        // ((1 + 2) + 3) * (4 - 5) = -6
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val d = rule {
                    case n : Num => n
                }
        val e = rule {
                    case Add (Num (i), Num (j)) => Num (i + j)
                    case Sub (Num (i), Num (j)) => Num (i - j)
                    case Mul (Num (i), Num (j)) => Num (i * j)
                }
        val s = alltdfold (d, e)
        expectResult (Some (Num (-6))) (s (t))
    }

    test ("restore restores when the strategy fails") {
        val t = Add (Num (1), Num (2))
        var count = 0
        val d = rule {
                    case n => count = count + 1; n
                } <* rule {
                    case Num (i) => Num (i + 1)
                }
        val e = rule {
                    case n => count = count - 1; n
                }
        val s = restore (d, e)
        expectResult (None) (s (t))
        expectResult (0) (count)
    }

    test ("restore doesn't restore when the strategy succeeds") {
        val t = Add (Num (1), Num (2))
        var count = 0
        val d = rule {
                    case n => count = count + 1; n
                }
        val e = rule {
                    case n => count = count - 1; n
                }
        val s = restore (d, e)
        expectsame (Some (t)) (s (t))
        expectResult (1) (count)
    }

    test ("restorealways restores when the strategy fails") {
        val t = Add (Num (1), Num (2))
        var count = 0
        val d = rule {
                    case n => count = count + 1; n
                } <* rule {
                    case Num (i) => Num (i + 1)
                }
        val e = rule {
                    case n => count = count - 1; n
                }
        val s = restorealways (d, e)
        expectResult (None) (s (t))
        expectResult (0) (count)
    }

    test ("restorealways restores when the strategy succeeds") {
        val t = Add (Num (1), Num (2))
        var count = 0
        val d = rule {
                    case n => count = count + 1; n
                }
        val e = rule {
                    case n => count = count - 1; n
                }
        val s = restorealways (d, e)
        expectsame (Some (t)) (s (t))
        expectResult (0) (count)
    }

    test ("lastly applies the second strategy when the first strategy fails") {
        val t = Add (Num (1), Num (2))
        var count = 0
        val d = rule {
                    case n => count = count + 1; n
                } <* rule {
                    case Num (i) => Num (i + 1)
                }
        val e = rule {
                    case n => count = count - 1; n
                }
        val s = lastly (d, e)
        expectResult (None) (s (t))
        expectResult (0) (count)
    }

    test ("lastly applies the second strategy when the first strategy succeeds") {
        val t = Add (Num (1), Num (2))
        var count = 0
        val d = rule {
                    case n => count = count + 1; n
                }
        val e = rule {
                    case n => count = count - 1; n
                }
        val s = lastly (d, e)
        expectsame (Some (t)) (s (t))
        expectResult (0) (count)
    }

    test ("ior applies second strategy if first strategy fails") {
        val t = Add (Num (1), Num (2))
        val u = Add (Num (2), Num (1))
        val d = rule {
                    case Num (i) => Num (i + 1)
                }
        val e = rule {
                    case Add (l, r) => Add (r, l)
                }
        val s = ior (d, e)
        expectResult (Some (u)) (s (t))
    }

    test ("ior applies second strategy if first strategy succeeds") {
        val t = Add (Num (1), Num (2))
        val u = Add (Num (9), Num (8))
        val d = rule {
                    case Add (l, r) => Add (Num (8), Num (9))
                }
        val e = rule {
                    case Add (l, r) => Add (r, l)
                }
        val s = ior (d, e)
        expectResult (Some (u)) (s (t))
    }

    test ("or applies second strategy and restores term if first strategy fails") {
        val t = Add (Num (1), Num (2))
        val d = rule {
                    case Num (i) => Num (i + 1)
                }
        val e = rule {
                    case Add (l, r) => Add (r, l)
                }
        val s = or (d, e)
        expectsame (Some (t)) (s (t))
    }

    test ("or applies second strategy and restores term if first strategy succeeds") {
        val t = Add (Num (1), Num (2))
        val d = rule {
                    case Add (l, r) => Add (Num (8), Num (9))
                }
        val e = rule {
                    case Add (l, r) => Add (r, l)
                }
        val s = or (d, e)
        expectsame (Some (t)) (s (t))
    }

    test ("and fails if the first strategy fails") {
        val t = Add (Num (1), Num (2))
        val d = rule {
                    case Num (i) => Num (i + 1)
                }
        val e = rule {
                    case Add (l, r) => Add (r, l)
                }
        val s = and (d, e)
        expectResult (None) (s (t))
    }

    test ("and fails if the first strategy succeeds but the second strategy fails") {
        val t = Add (Num (1), Num (2))
        val d = rule {
                    case Add (l, r) => Add (r, l)
                }
        val e = rule {
                    case Num (i) => Num (i + 1)
                }
        val s = and (d, e)
        expectResult (None) (s (t))
    }

    test ("and succeeds and restores term if both strategies succeed") {
        val t = Add (Num (1), Num (2))
        val d = rule {
                    case Add (l, r) => Add (Num (8), Num (9))
                }
        val e = rule {
                    case Add (l, r) => Add (r, l)
                }
        val s = and (d, e)
        expectsame (Some (t)) (s (t))
    }

    {
        val t = Mul (Add (Add (Num (1), Num (2)), Num (3)), Sub (Num (4), Num (5)))
        val u = Mul (Add (Add (Num (12), Num (13)), Num (14)), Sub (Num (16), Num (17)))

        test ("everywhere traverses in expected order") {
            var l : List[Double] = Nil
            var count = 9
            val r = rule {
                        case Num (i) => l = l :+ i
                                        Num (count)
                        case n       => count = count + 1
                                        n
                    }
            val s = everywhere (r)
            expectResult (Some (u)) (s (t))
            expectResult (List (1, 2, 3, 4, 5)) (l)
        }

        test ("everywheretd traverses in expected order") {
            var l : List[Double] = Nil
            var count = 9
            val r = rule {
                        case Num (i) => l = l :+ i
                                        Num (count)
                        case n       => count = count + 1
                                        n
                    }
            val s = everywheretd (r)
            expectResult (Some (u)) (s (t))
            expectResult (List (1, 2, 3, 4, 5)) (l)
        }

        test ("everywherebu traverses in expected order") {
            var l : List[Double] = Nil
            var count = 9
            val r = rule {
                        case Num (i) => l = l :+ i
                                        Num (count)
                        case n       => count = count + 1
                                        n
                    }
            val s = everywheretd (r)
            expectResult (Some (u)) (s (t))
            expectResult (List (1, 2, 3, 4, 5)) (l)
        }
    }

    // Strategy naming tests

    val myrule1 = rule {
        case Num (i) => Num (i + 1)
    }

    test ("class-level rule has the correct name") {
        expectResult ("myrule1") (myrule1.name)
    }

    test ("rule in closure has the correct name") {
        val myrule2 = rule {
            case Num (i) => Num (i + 1)
        }
        expectResult ("myrule2") (myrule2.name)
    }

    val mystrategy1 = outermost (id)

    test ("class-level strategy has the correct name") {
        expectResult ("mystrategy1") (mystrategy1.name)
    }

    test ("strategy in closure has the correct name") {
        val mystrategy2 = outermost (id)
        expectResult ("mystrategy2") (mystrategy2.name)
    }

}
