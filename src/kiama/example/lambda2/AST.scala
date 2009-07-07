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
 * A simple lambda calculus abstract syntax.
 */
object AST {
  
    import kiama.attribution.Attribution._
  
    /**
     * Identifiers are represented as strings.
     */
    type Idn = String
    
    /**
     * Expressions.
     */
    abstract class Exp extends Product with Attributable
    
    /**
     * Numeric expressions.
     */
    case class Num (value : Int) extends Exp {
        override def toString = value.toString
    }
    
    /**
     * Variable expressions.
     */    
    case class Var (name : Idn) extends Exp {
        override def toString = name
    }
    
    /**
     * Lambda expressions binding name of type tipe within body.
     */
    case class Lam (name : Idn, tipe : Type, body : Exp) extends Exp {
        override def toString = "(\\" + name + " : " + tipe + " . " + body + ")"
    }
    
    /**
     * Application of l to r.
     */
    case class App (l : Exp, r : Exp) extends Exp {
        override def toString = "(" + l + " " + r + ")"
    }
    
    /**
     * An application of a primitive binary operation.
     */
    case class Opn (op : Op, left : Exp, right : Exp) extends Exp {
        override def toString = "(" + left + " " + op + " " + right + ")"
    }
    
    /**
     * Bind name of type tipe to the value of exp in body.
     */
    case class Let (name : Idn, tipe : Type, exp : Exp, body : Exp) extends Exp {
        override def toString = "(let " + name + " : " + tipe + " = " + exp + " in " + body + ")"
    }

    /**
     * Types.
     */
    abstract class Type extends Product with Attributable
    
    /**
     * Primitive integer type.
     */
    case object IntType extends Type {
        override def toString = "Int"
    }
    
    /**
     * Function type from an argument type arg to a result type res.
     */
    case class FunType (arg : Type, res : Type) extends Type {
        override def toString = "" + arg + " -> " + res
    }

    /**
     * Primitive binary operators.
     */
    abstract class Op extends Attributable {
    	/**
         * Evaluate the oeprator on the given integer operands.
         */
        def eval (l : Int, r : Int) : Int
    }
    
    /**
     * Primitive integer addition.
     */
    case object AddOp extends Op {
        def eval (l : Int, r : Int) = l + r
        override def toString = "+"
    }
    
    /**
     * Primitive integer subtraction.
     */
    case object SubOp extends Op {
        def eval (l : Int, r : Int) = l - r
        override def toString = "-"
    }
    
}
