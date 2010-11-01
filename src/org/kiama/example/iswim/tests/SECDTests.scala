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
package example.iswim.tests

/*
 * Tests of the basic SECD machine.
 */

// TODO: much more exhaustive tests of the SECD machine.

import org.kiama.example.iswim.secd._
import org.kiama.util.PrettyPrinter
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SECDTests extends FunSuite {

    import SECDBase._
    import IntegerOps._
    import IntComparisonOps._
    import StackOps._
    import BooleanOps._
    import HeapOps._
    import IOOps._
    import StringOps._
    import RecordOps._

    private class SECD(code : Code) extends SECDBase
        with IntegerOps with BooleanOps
        with IntComparisonOps with StackOps
        with HeapOps with IOOps with StringOps
        with RecordOps {

        /**
         * Table of builtin primitives for this machine.
         * These named values may be loaded into the environment
         * using the BindPrims(nms) bytecode.
         */

        def primTable : Map[Name,Value] = Map(
            "three" -> IntValue(3)
        ,   "five" -> IntValue(5)
        ,   "seven" -> IntValue(7)
        ,   "eleven" -> IntValue(11)
        )

        /**
         * Wrap supplied code in a preamble / postamble
         */
        private val program : CodeSegment = CodeSegment(
            MkClosures(
                List(FunctionSpec(None,"@exnHandler", CodeSegment(code)))
            )
         ,    AppCC()
         )

        /**
        * Initialise the machine.
        */
        override def init () = {
            stack := Nil
            envir := Map()
            dump := EmptyCont
            control := program.code
        }
    }

    test("just run the machine with no code") {
        val machine = new SECD(List()) {
            def verify {
                assert(stack.value === List(UnexpectedTermination))
                assert(envir.value.isEmpty)
                assert(control.value.isEmpty)
                assert(dump.value === EmptyCont)
            }
        }
        machine.run
        machine.verify
    }

    test("test that Enter() binds variables in the correct order") {
        val machine = new SECD(List(
            PushInt(3), PushInt(5), PushInt(7), Enter(List("x","y","z")),
            Lookup("y"), Lookup("x"), Lookup("z"), Lookup("x"), MkRecord(4), Exit()
        )) {
            def verify {
                assert(stack.value === List(RecordValue(
                    List(IntValue(3),IntValue(7),IntValue(3),IntValue(5)))))
                assert(envir.value.isEmpty)
                assert(control.value.isEmpty)
                assert(dump.value === EmptyCont)
            }
        }
        machine.run
        machine.verify
    }

    test("test that BindPrims() binds variables correctly") {
        val machine = new SECD(List(
            BindPrims(List("three","five","seven","eleven")),
            Lookup("seven"), Lookup("three"), Lookup("eleven"), Lookup("five"),
            MkRecord(4), Exit()
        )) {
            def verify {
                assert(stack.value === List(RecordValue(
                    List(IntValue(5),IntValue(11),IntValue(3),IntValue(7)))))
                assert(envir.value.isEmpty)
                assert(control.value.isEmpty)
                assert(dump.value === EmptyCont)
            }
        }
        machine.run
        machine.verify
    }

    test("test that an error is generated if we try to bind a non-existent primitive") {
        val machine = new SECD(List(
            BindPrims(List("one")), Lookup("one"), Exit()
        )) {
            def verify {
                assert(stack.value === List(NonExistentPrimitive))
                assert(envir.value.isEmpty)
                assert(control.value.isEmpty)
                assert(dump.value === EmptyCont)
            }
        }
        machine.run
        machine.verify
    }

    test("the simplest possible correct program - simply return ()") {
        val machine = new SECD(List(PushEmpty())) {
            def verify {
                assert(stack.value === List(EmptyValue))
                assert(envir.value.isEmpty)
                assert(control.value.isEmpty)
                assert(dump.value === EmptyCont)
            }
        }
        machine.run
        machine.verify
    }

    test("a while loop to sum the numbers 0 to 20") {
        val machine = new SECD(List(
            Alloc(),
            Dup(1),
            PushInt(0),
            Put(),
            Enter(List("@counter")),
            Alloc(),
            Dup(1),
            PushInt(0),
            Put(),
            Enter(List("@sum")),
            MkClosures(List(
                FunctionSpec(None,"@loop",CodeTree(
                    Lookup("@counter"),
                    Get(),
                    PushInt(20),
                    LessThanOrEqual(),
                    Test(
                        CodeTree(
                            Lookup("@sum"),
                            Dup(1),
                            Get(),
                            Lookup("@counter"),
                            Get(),
                            Add(),
                            Put(),
                            Lookup("@counter"),
                            Dup(1),
                            Get(),
                            PushInt(1),
                            Add(),
                            Put(),
                            Lookup("@loop"),
                            Dup(1),
                            TailApp()
                        ),
                        CodeTree(
                            Lookup("@sum"),
                            Get()
                        )
                    )
                ))
            )),
            Dup(1),
            App(),
            Exit()
        )) {
            def verify {
                assert(stack.value === List(IntValue(210)))
                assert(envir.value.isEmpty)
                assert(control.value.isEmpty)
                assert(dump.value === EmptyCont)
            }
        }
        machine.run
        machine.verify
    }

    test("our old friend - the factorial function") {
        val machine = new SECD(List(
            PushInt(10),
            MkClosures(List(
                FunctionSpec(
                    Some("factorial"),
                    "n",
                    CodeSegment(
                        Lookup("n"),
                        PushInt(0),
                        LessThanOrEqual(),
                        Test(
                            CodeSegment(
                                PushInt(1)
                            ),
                            CodeSegment(
                                Lookup("n"),
                                Dup(1),
                                PushInt(1),
                                Sub(),
                                Lookup("factorial"),
                                App(),
                                Mult()
                            )
                        )
                    )
                )
            )),
            App()
        )) {
            def verify {
                assert(stack.value === List(IntValue(3628800)))
                assert(envir.value.isEmpty)
                assert(control.value.isEmpty)
                assert(dump.value === EmptyCont)
            }
        }
        machine.run
        machine.verify
    }

    test("test handling of a string literal containing escapes") {
        val machine = new SECD(List(PushString("""\"hello\t\\there\"\n"""))) {
                def verify {
                    assert(stack.value === List(StringValue("\"hello\t\\there\"\n")))
                    assert(envir.value.isEmpty)
                    assert(control.value.isEmpty)
                    assert(dump.value === EmptyCont)
                }
            }
        machine.run
        machine.verify
    }

}
