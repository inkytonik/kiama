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

import junit.framework.TestCase
import org.scalatest.junit.JUnit3Suite 
import org.scalatest.prop.Checkers 
import kiama.parsing.CharPackratParsers

class TIL1_1Tests extends TestCase with JUnit3Suite with Checkers {
    
    import AST._
    import TIL1_1Main._
            
    /**
     * Make sure that the Factorial program parses to what we expect.
     */
    def testFactorialParse {
        val input = """
var n;
read n;
var x;
var fact;
fact := 1;
for x := 1 to n do
    fact := x * fact;
end
write "factorial of ";
write n;
write " is ";
write fact;
write "\n";"""
        val tree =
            Program (
                List (
                    Decl (Id ("n")),
                    Read (Id ("n")),
                    Decl (Id ("x")),
                    Decl (Id ("fact")),
                    Assign (Id ("fact"), Num (1)),
                    For (Id ("x"), Num (1), Var (Id ("n")),
                        List (
                            Assign (Id ("fact"), Mul (Var (Id ("x")), Var (Id ("fact")))))),
                    Write (Str ("factorial of ")),
                    Write (Var (Id ("n"))),
                    Write (Str (" is ")),
                    Write (Var (Id ("fact"))),
                    Write (Str ("\\n"))))
        test (input, tree)
    }
    
    def testFactorsParse {
        val input = """
var n;
write "Input n please";
read n;
write "The factors of n are";
var f;
f := 2;
while n != 1 do
    while (n / f) * f = n do
        write f;
        n := n / f;
    end
    f := f + 1;
end"""
        val tree =
            Program (
                List (
                    Decl (Id ("n")),
                    Write (Str ("Input n please")),
                    Read (Id ("n")),
                    Write (Str ("The factors of n are")),
                    Decl (Id ("f")),
                    Assign (Id ("f"), Num (2)),
                    While (Ne (Var (Id ("n")), Num (1)),
                        List (
                            While (Eq (Mul (Div (Var (Id ("n")), Var (Id ("f"))), Var (Id ("f"))), Var (Id ("n"))),
                                List (
                                    Write (Var (Id ("f"))),
                                    Assign (Id ("n"), Div (Var (Id ("n")), Var (Id ("f")))))),
                            Assign (Id ("f"), Add (Var (Id ("f")), Num (1)))))))
        test (input, tree)
    }
    
    def testMultiplesParse {
        val input = """
for i := 1 to 9 do
    for j := 1 to 10 do
        write i*j;
    end
end
"""
        val tree =
            Program (
                List (
                    For (Id ("i"), Num (1), Num (9),
                        List (
                            For (Id ("j"), Num (1), Num (10),
                                List (Write (Mul (Var (Id ("i")), Var (Id ("j"))))))))))
        test (input, tree)
    }

}
