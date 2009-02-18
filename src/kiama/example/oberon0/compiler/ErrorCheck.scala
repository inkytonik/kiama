/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
 *
 * Contributed by Ben Mockler.
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

package kiama.example.oberon0.compiler

import kiama.attribution.Attribution._

object ErrorCheck {

    import AST._
    import NameAnalysis._
    import TypeAnalysis._
    import ValueAnalysis._
    import ConstantAnalysis._

    val collectErrors : Attributable ==> List[String] =
        attr {
            case obj => {
                var errs : List[String] = Nil

                // Process the errors of the children of t
		        for (child <- obj.children)
                    errs = errs ::: collectErrors(child)

		        // Process the errors at t
		        obj match {
		
		            // Check for name-mismatch errors in module declarations
		            case md @ ModuleDecl (id, _, _, id2, _) if id.name != id2.name => {
                        errs = ("Name mismatch: Opening identifier = " + id.name + ", Closing identifier = " + id2.name) :: errs
		            }

		            // Check for name-mismatch errors in procedure declarations
		            case pd @ ProcDecl (id, _, _, _, id2, _) if id.name != id2.name => {
                        errs = ("Name mismatch: Opening identifier = " + id.name + ", Closing identifier = " + id2.name) :: errs
		            }

		            // Check for undeclared identifiers (applied occurrences only)
		            case id @ Ident (nm) => {
		                if (!(id.parent.isInstanceOf[Declaration]))
		                    if ((id->decl).isInstanceOf[UnknownDecl])
                                errs = ("Declaration not found: " + nm) :: errs
		            }

		            // Check for duplicate declarations
		            case dec : Declaration if dec->isMultiplyDefined => {
    	                errs = ("Duplicate declaration = " + dec.getId.name) :: errs
    	            }

                    // Check for incompatible types on either side of assignment statement
                    case as @ Assignment (desig, exp) if as->objType == InvalidType => {
		                errs = ("Type mismatch in assignment expression: LHS is " + (desig->objType).toString
                            + ", RHS is " + (exp->objType).toString) :: errs
    	            }

                    // Check for assignable expression on LHS of assignment statement
                    case as @ Assignment (desig, _) if !(desig->isAssignable) => {
		                errs = ("LHS of := is not assignable: " + desig) :: errs
    	            }

                    // Check for non-integer size expression in array type declaration
		            case at @ ArrayType (sz, _) if sz->objType !=  IntegerType => {
                        errs = ("Non-integer array size expression: " + at) :: errs
    	            }

                    // Check for non-constant size expression in array type declaration
		            case at @ ArrayType (sz, _) if !(sz->isConstant) => {
                        errs = ("Non-constant array size expression: " + at) :: errs
    	            }

                    // Check for negative size expression in array type declaration
		            case at @ ArrayType (sz, _) if sz->intValue < 0 => {
                        errs = ("Negative array size expression: " + at) :: errs
    	            }              
              
                    // Check for non-integer index expression in array designation
		            case ad @ ArrayDesig (_, exp) if exp->objType !=  IntegerType => {
                        errs = ("Non-integer array index expression: " + ad) :: errs
    	            }

                    // Check for constant, negative index expression in array designation
		            case ad @ ArrayDesig (_, exp) if (exp->isConstant && (exp->intValue < 0)) => {
                        errs = ("Negative array index expression: " + ad) :: errs
    	            }

                    // Check for constant, out-of-bounds index expression in array designation
		            case ad @ ArrayDesig (left, exp) if exp->isConstant => {
                        left->objType match {
                            case ArrayType (sz, _) => {
                                if (exp->intValue >= sz->intValue)
                                    errs = ("Out-of-bounds array index expression: " + ad) :: errs
                            }
                            case _ => errs = ("Error processing: " + ad) :: errs
                        }
    	            }

                    // Check procedure call is on an actual procedure
                    case pc @ ProcedureCall (desig, _) if !(desig->objType).isInstanceOf[ProcType] => {
                        errs = (desig + " is not a procedure") :: errs
                    }

                    // Check procedure call actual params against formal params
                    case pc : ProcedureCall => {
                        errs = pc->procArgErrors ::: errs
                    }

                    // Check If-statement expressions are boolean
                    case is @ IfStatement (condexp, _, _) if condexp->objType != BooleanType => {
                        errs = (condexp + " must be a boolean expression") :: errs
                    }

                    // Check While-statement expressions are boolean
                    case ws @ WhileStatement (condexp, _) if condexp->objType != BooleanType => {
                        errs = (condexp + " must be a boolean expression") :: errs
                    }
                    
                    // Rule out using procedures with field designators (temporarily?)
                    // On LHS
                    case fd @ FieldDesig (left, id) if (left->objType).isInstanceOf[ProcType] => {
                        errs = (fd + "Procedure calls cannot use field designators") :: errs
                    }

                    // On RHS
                    case fd : FieldDesig if (fd->objType).isInstanceOf[ProcType] => {
                        errs = (fd + "Procedure calls cannot use field designators") :: errs
                    }

                    // Rule out using procedures with array designators (temporarily?)
                    case ad : ArrayDesig if (ad->objType).isInstanceOf[ProcType] => {
                        errs = (ad + "Procedure calls cannot use array designators") :: errs
                    }

                    case _ => ()
		        }

		        errs
	        }
        }
}
