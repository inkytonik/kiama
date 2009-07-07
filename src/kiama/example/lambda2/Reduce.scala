/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
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

package kiama.example.lambda2

/**
 * Evaluation of lambda calculus using global beta reduction with meta-level
 * substitution and arithmetic operations.
 */
trait Reduce extends Evaluator {
  
	import AST._

    /**
     * Evaluate by repeatedly trying to apply beta reduction and arithmetic
     * operators anywhere.
     */
    lazy val evals =
        reduce (beta + arithop)
    
    /**
     * Beta reduction via meta-level substitution.
     */
    lazy val beta =
    	rule {
    	  case App (Lam (x, _, e1), e2) => substitute (x, e2, e1) 
    	}
    
    /*
     * Evaluation of arithmetic operators.
     */
    lazy val arithop =
        rule {
            case Opn (op, Num (l), Num (r)) => Num (op.eval (l, r))
        }

    /**
     * Capture-free substitution of free occurrences of x in e1 with e2.
     */
    def substitute (x : Idn, e2: Exp, e1 : Exp) : Exp =
    	e1 match {
    	    case Var (y) if x == y =>
    	        e2
            case Lam (y, t, e3) =>
                val z = freshvar ()
                Lam (z, t, substitute (x, e2, substitute (y, Var (z), e3)))
            case App (l, r) =>
                App (substitute (x, e2, l), substitute (x, e2, r))
            case Opn (op, l, r) =>
                Opn (op, substitute (x, e2, l), substitute (x, e2, r))
    	    case e =>
    	        e         
    	}
    
}

class ReduceEvaluator extends Reduce {
    override def reducesinlambdas = true
}
