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
 * Interface for an individual rewriting-based lambda2 evaluator.
 */
trait Evaluator extends kiama.rewriting.Rewriter {
  
	/**
     * Evaluate the given expression, returning the result of the
     * evaluation if it succeeded, or exp if it failed.
     */
    def eval (exp : AST.Exp) : AST.Exp =
        rewrite (evals) (exp)
        
    /**
     * The strategy to use to perform the evaluation.
     */
    val evals : Strategy

    /**
     * Whether this mechanism evaluates inside lambdas.  Used for
     * testing.  Default: false.
     */
    def reducesinlambdas = false
    
    /**
     * Generate a fresh variable name.  Prefix the name with an underscore
     * to avoid the potential for clashes with user-level varaibles (which
     * must start with a letter).
     */
    object freshvar {
        private var count = 0
        def apply () : String = {
            count = count + 1
            "_v" + count
        }
    }
    
}
