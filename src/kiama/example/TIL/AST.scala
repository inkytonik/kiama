/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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
                                
package kiama.example.til

/**
 * Abstract syntax for the Tiny Imperative Language.
 * http://www.program-transformation.org/Sts/TinyImperativeLanguage
 */
object AST {
    
    case class Program (ss : Seq[Stat])

    abstract case class Stat

    case class Decl (i : Id) extends Stat

    case class Assign (i : Id, e : Exp) extends Stat

    case class IfThen (e : Exp, t : Seq[Stat]) extends Stat
    case class IfElse (e : Exp, t : Seq[Stat], f : Seq[Stat]) extends Stat

    case class While (e : Exp, b : Seq[Stat]) extends Stat
    case class For (i : Id, f : Exp, t : Exp, b : Seq[Stat]) extends Stat

    case class Read (i : Id) extends Stat
    case class Write (e : Exp) extends Stat
    
    type Id = String
    
    abstract case class Exp
        
    case class Var (i : Id) extends Exp
    case class Num (n : Int) extends Exp
    case class Str (s : String) extends Exp
    
    case class Mul (l : Exp, r : Exp) extends Exp
    case class Div (l : Exp, r : Exp) extends Exp
    case class Add (l : Exp, r : Exp) extends Exp
    case class Sub (l : Exp, r : Exp) extends Exp
    
    case class Eq (l : Exp, r : Exp) extends Exp
    case class Ne (l : Exp, r : Exp) extends Exp

}
