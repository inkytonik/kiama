/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Dominic R B Verity, Macquarie University.
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
package example.iswim.driver

/**
 * A complete SECD machine implementation, with facilities for
 * specifying preloaded primitive code segments.
 */

import org.kiama.example.iswim.secd._

import SECDBase._

class SECD(code : CodeTree) extends SECDBase with ExceptionHandler
    with IntegerOps with BooleanOps with ConversionOps
    with IntComparisonOps with StackOps with RecordOps
    with HeapOps with IOOps with StringOps {

    import IntegerOps._
    import IntComparisonOps._
    import StackOps._
    import BooleanOps._
    import HeapOps._
    import IOOps._
    import StringOps._
    import ConversionOps._
    import RecordOps._
    import ExceptionHandler._

    /**
     * Table of builtin primitives for this machine.
     * These named values may be loaded into the environment
     * using the BindPrims(nms) bytecode.
     */

    def primTable : Map[Name,Value] = Map(
        // Type Values
        "EmptyTypeValue" -> EmptyTypeValue
    ,   "TypeTypeValue" -> TypeTypeValue
    ,   "ClosureTypeValue" -> ClosureTypeValue
    ,   "ContTypeValue" -> ContTypeValue
    ,   "ExceptionTypeValue" -> ExceptionTypeValue
    ,   "StringTypeValue" -> StringTypeValue
    ,   "IntTypeValue" -> IntTypeValue
    ,   "RecordTypeValue" -> RecordTypeValue
    ,   "BooleanTypeValue" -> BooleanTypeValue
    ,   "PrimTypeValue" -> PrimTypeValue
        // Machine Exception Values
    ,   "UnboundVariable" -> UnboundVariable
    ,   "StackUnderflow" -> StackUnderflow
    ,   "TypeError" -> TypeError
    ,   "UnexpectedTermination" -> UnexpectedTermination
    ,   "UnexpectedExit" -> UnexpectedExit
    ,   "DumpEmpty" -> DumpEmpty
    ,   "MalformedInstruction" -> MalformedInstruction
    ,   "MatchError" -> MatchError
    ,   "FieldOutOfBounds" -> FieldOutOfBounds
    ,   "DivisionByZero" -> DivisionByZero
    ,   "ConversionError" -> ConversionError
    ,   "NonExistentPrimitive" -> NonExistentPrimitive
        // Builtin Functions
    ,   "write" -> PrimValue(List(Write(),PushEmpty()))
                                                    // write a value to the terminal
    ,   "read" -> PrimValue(List(Pop(1),Read()))    // read a string from the terminal
    ,   "type" -> PrimValue(List(GetType()))        // get the type of a value
    ,   "fields" -> PrimValue(List(                 // get the number of fields in a value
            Dup(1), GetType(), Dup(1),
            PushType(RecordTypeValue),
            Equals(), Swap(1,1),
            PushType(EmptyTypeValue),
            Equals(),
            Test(
                CodeSegment(Pop(2),PushInt(0)),
                Test(Fields(),CodeSegment(Pop(1),PushInt(1)))
            )))
    ,   "newException" -> PrimValue(List(MkUserException()))
                                        // create a new user exception value from a string
    ,   "raise" -> PrimValue(List(RaiseException()))
                                        // raise an exception.
    ,   "setHandler" -> PrimValue(List(SetHandler(),PushEmpty()))
                                        // set the current exception handler
    ,   "toInt" -> PrimValue(List(ToInt()))
    ,   "toString" -> PrimValue(List(ToString()))
    ,   "toBoolean" -> PrimValue(List(ToBoolean()))
        // The exception handler reference
    ,   "exnHandler" -> RefValue()
    )

    /**
     * Wrap supplied code in a preamble / postamble
     * This wrapper ensures that the exception handler
     * register is initialised to contain a global
     * "abort" exception handler.
     */
    private val program : CodeSegment = CodeSegment(
        MkClosures(List(
      	    FunctionSpec(
      	        None
      	    ,   "@globalExnHandler"
      	    ,   CodeTree(
      	            Lookup("@globalExnHandler")
      	        ,   SetHandler()
      	        ,   CodeTree(code)
      	        ))
      	))
    ,	AppCC()
    )

    /**
     * Initialise the machine.
     */
    override def init () = {
        stack := Nil
        envir := Map()
        dump := EmptyCont
        handler := EmptyCont
        control := program.code
    }
}


