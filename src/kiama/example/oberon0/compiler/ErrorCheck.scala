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

import kiama.attribution.Attributable
import kiama.attribution.Attribution._

/**
 * ErrorCheck:  Performs semantic analysis of a program.
 */
object ErrorCheck {

    import AST._
    import NameAnalysis._
    import TypeAnalysis._
    import ValueAnalysis._
    import ConstantAnalysis._

    /**
     * Attribute 'collectErrors':  A list of all the semantic errors found
     */
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
                    case md @ ModuleDecl (nm, _, _, nm2, _) if nm != nm2 => {
                            errs = ("Name mismatch: Opening identifier = " + nm + ", Closing identifier = " + nm2) :: errs
                    }

                    // Check for name-mismatch errors in procedure declarations
                    case pd @ ProcDecl (nm, _, _, _, nm2, _) if nm != nm2 => {
                            errs = ("Name mismatch: Opening identifier = " + nm + ", Closing identifier = " + nm2) :: errs
                    }

                    // Check for undeclared identifiers (applied occurrences only)
                    case id @ Ident (nm) => {
                        if ((id->decl).isInstanceOf[UnknownDecl])
                                errs = ("Declaration not found: " + nm) :: errs
                    }

                    // Check for duplicate declarations
                    case dec : Declaration if dec->isMultiplyDefined => {
                        errs = ("Duplicate declaration = " + dec.getName) :: errs
                    }

                    // Check for incompatible types on either side of assignment statement
                    case as @ Assignment (desig, exp) if as->objType == InvalidType => {
                        errs = ("Type mismatch in assignment expression: LHS is " + (desig->objType).toString
                            + ", RHS is " + (exp->objType).toString) :: errs
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
                    case ProcedureCall (desig, _) if !(desig->objType).isInstanceOf[ProcType] => {
                        errs = (desig + " is not a procedure") :: errs
                    }

                    // The procedure name must be a plain identifier
                    case pc @ ProcedureCall (desig, _) => {

                        desig match {
                            case fd : FieldDesig => {
                                errs = (fd + "Procedure calls cannot use field designators") :: errs
                            }
                            case ad : ArrayDesig => {
                                errs = (ad + "Procedure calls cannot use array designators") :: errs
                            }
                            case id : Ident => {
                                // The procedure called must be a child or sibling of the
                                // current procedure
                                if (!(id->decl).isInstanceOf[BuiltInProcDecl])
                                    if (id->decl->level < id->level)
                                        errs = (pc + " is not a child or sibling of " + pc.parent) :: errs

                                // Check procedure call actual params against formal params
                                errs = pc->procArgErrors ::: errs
                            }

                            case _ => ()
                        }
                    }

                    // Check If-statement expressions are boolean
                    case IfStatement (condexp, _, _) if condexp->objType != BooleanType => {
                        errs = (condexp + " must be a boolean expression") :: errs
                    }

                    // Check While-statement expressions are boolean
                    case ws @ WhileStatement (condexp, _) if condexp->objType != BooleanType => {
                        errs = (condexp + " must be a boolean expression") :: errs
                    }

                    case _ => ()
            }

            errs
        }
    }
}
