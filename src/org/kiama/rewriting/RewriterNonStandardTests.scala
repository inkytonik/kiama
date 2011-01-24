/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011 Anthony M Sloane, Macquarie University.
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

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
 * Rewriting tests.
 */
@RunWith(classOf[JUnitRunner])
class RewriterNonStandardTests extends FunSuite with Checkers {

    import org.kiama.example.imperative.ASTNonCase._
    import org.kiama.rewriting.Rewriter.{fail => rwfail, _}

    {
        // (abc + 1) * (xyz - 3)
        val p = new Mul (new Add (new Var ("abc"), new Num (1.0)),
                         new Sub (new Var ("xyz"), new Num (3.0)))
        
        // Incr Nums, reverse Vars, turn Adds into Sub, swap Add args
        val r = rule {
                    case n : Num => new Num (n.d + 1)
                    case v : Var => new Var (v.s.reverse)
                    case a : Add => new Sub (a.r, a.l)
                }
        // Canonicalise variables
        val s = rule {
                    case v : Var => new Var ("varname")
                }        
        
        test ("rewrite normal classes: top-level fail") {
            expect (None) (r (p))
        }
        
        test ("rewrite normal classes: all") {
            // (1 - abc) * (zyx - 4)
            expect ("Some(Mul(Sub(Num(1.0),Var(abc)),Sub(Var(zyx),Num(4.0))))") (
                ((alltd (r)) (p)).toString
            )
        }
        
        test ("rewrite normal classes: some") {
            // (varname + 1) * (varname - 3)
            expect ("Some(Mul(Add(Var(varname),Num(1.0)),Sub(Var(varname),Num(3.0))))") (
                ((sometd (s)) (p)).toString
            )
        }
        
        test ("rewrite normal classes: one") {
            // (varname + 1) * (xyz - 3)
            expect ("Some(Mul(Add(Var(varname),Num(1.0)),Sub(Var(xyz),Num(3.0))))") (
                ((oncetd (s)) (p)).toString
            )
        }
        
        test ("rewrite normal classes: counting all terms using count") {
            val countall = count { case _ => 1 }
            expect (11) (countall (p))
        }
        
        test ("rewrite normal classes: counting all terms using a para") {
            val countfold = 
                para[Int] {
                    case (t, cs) => 1 + cs.sum
                }
            expect (11) (countfold (p))
        }
    }
    
}

