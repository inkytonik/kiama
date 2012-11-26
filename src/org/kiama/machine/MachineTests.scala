/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package machine

import org.kiama.util.Tests
import org.scalatest.junit.JUnitRunner

/**
 * Basic tests of machine module.  More complex setups are tested
 * within particular examples.
 */
class MachineTests extends Tests {

    import org.kiama.util.StringEmitter

    val memitter = new StringEmitter ()

    object M extends Machine ("M", memitter) {
        override val debug = true
        def main { }
    }

    // Scalar state

    test ("new state is undefined") {
        val s = new M.State[Int] ("s")
        expectResult (true) (s.isUndefined)
    }

    test ("asking for the value of undefined state gives an error") {
        val s = new M.State[Int] ("s")
        val i = intercept[RuntimeException] {
                    s.value
                }
        expectResult ("State.value: M.s is undefined") (i.getMessage)
    }

    test ("state can be made undefined") {
        val s = new M.State[Int] ("s")
        M.reset
        s := 42
        M.performUpdates
        expectResult (false) (s.isUndefined)
        s.undefine
        expectResult (true) (s.isUndefined)
    }

    test ("undefined state is not equal to anything") {
        val s = new M.State[Int] ("s")
        expectResult (false) (s =:= 0)
        expectResult (false) (s =:= 42)
        expectResult (false) (s =:= 99)
    }

    test ("defined state is only equal to its value") {
        val s = new M.State[Int] ("s")
        s := 0
        M.performUpdates
        expectResult (true) (s =:= 0)
        expectResult (false) (s =:= 42)
        expectResult (false) (s =:= 99)
    }

    test ("undefined state toStrings to a special message") {
        val s = new M.State[Int] ("s")
        expectResult ("** undefined **") (s.toString)
    }

    test ("defined state toStrings to its value") {
        val s = new M.State[Int] ("s")
        M.reset
        s := 42
        M.performUpdates
        expectResult ("42") (s.toString)
    }

    test ("state updates trigger suitable debug messages") {
        val s = new M.State[Int] ("s")
        val t = new M.State[Int] ("t")
        memitter.clear
        M.reset
        s := 88
        t := 99
        M.performUpdates
        M.reset
        s := 44
        M.performUpdates
        expectResult ("M.t := 99\nM.s := 88\nM.s := 44\n") (memitter.result)
    }

    test ("multiple consistent state updates are allowed") {
        val s = new M.State[Int] ("s")
        M.reset
        s := 0
        s := 0
        M.performUpdates
        expectResult (true) (s =:= 0)
        expectResult (false) (s =:= 1)
    }

    test ("inconsistent state updates in differents steps are allowed") {
        val s = new M.State[Int] ("s")
        M.reset
        s := 0
        M.performUpdates
        expectResult (true) (s =:= 0)
        expectResult (false) (s =:= 1)
        M.reset
        s := 1
        M.performUpdates
        expectResult (false) (s =:= 0)
        expectResult (true) (s =:= 1)
    }

    test ("inconsistent state updates in one step trigger an exception") {
        val s = new M.State[Int] ("s")
        M.reset
        s := 0
        s := 1
        val i = intercept[InconsistentUpdateException[Int]] {
                    M.performUpdates
                }
        expectResult ("Machine = M, update = M.s := 0, other value = 1") (i.getMessage)
    }

    // Parameterised state

    test ("new parameterised state is undefined") {
        val p = new M.ParamState[Int,Int] ("p")
        expectResult (true) (p.isUndefined (0))
        expectResult (true) (p.isUndefined (42))
        expectResult (true) (p.isUndefined (99))
    }

    test ("asking for the value of undefined parameterised state gives an error") {
        val p = new M.ParamState[Int,Int] ("p")
        val i = intercept[RuntimeException] {
                    p.value (0)
                }
        expectResult ("ParamState.value: M.p is undefined") (i.getMessage)
    }

    test ("asking for the value of parameterised state at an undefined value gives an error") {
        val p = new M.ParamState[Int,Int] ("p")
        M.reset
        p (0) := 42
        M.performUpdates
        val i = intercept[RuntimeException] {
                    p.value (12)
                }
        expectResult ("ParamState.value: M.p(12) is undefined") (i.getMessage)
    }

    test ("parameterised state can be made undefined") {
        val p = new M.ParamState[Int,Int] ("p")
        M.reset
        p (0) := 42
        M.performUpdates
        expectResult (false) (p.isUndefined (0))
        p.undefine (0)
        expectResult (true) (p.isUndefined (0))
    }

    test ("undefined parameterised state is not equal to anything") {
        val p = new M.ParamState[String,Int] ("p")
        expectResult (false) (p ("one") =:= 0)
        expectResult (false) (p ("one") =:= 42)
        expectResult (false) (p ("one") =:= 99)
    }

    test ("defined parameterised state is only equal to its value") {
        val p = new M.ParamState[String,Int] ("p")
        M.reset
        p ("one") := 42
        M.performUpdates
        expectResult (false) (p ("one") =:= 0)
        expectResult (true) (p ("one") =:= 42)
        expectResult (false) (p ("one") =:= 99)
        M.reset
        p ("two") := 99
        M.performUpdates
        expectResult (false) (p ("one") =:= 0)
        expectResult (true) (p ("one") =:= 42)
        expectResult (false) (p ("one") =:= 99)
        expectResult (false) (p ("two") =:= 0)
        expectResult (false) (p ("two") =:= 42)
        expectResult (true) (p ("two") =:= 99)
    }

    test ("parameterised state updates trigger suitable debug messages") {
        val p = new M.ParamState[String,Int] ("p")
        val q = new M.ParamState[Int,Int] ("q")
        memitter.clear
        M.reset
        p ("one") := 1
        p ("two") := 2
        q (0) := 0
        M.performUpdates
        M.reset
        p ("one") := 3
        q (0) := 1
        q (1) := 2
        M.performUpdates
        expectResult ("""M.q(0) := 0
                  |M.p(two) := 2
                  |M.p(one) := 1
                  |M.q(1) := 2
                  |M.q(0) := 1
                  |M.p(one) := 3
                  |""".stripMargin) (memitter.result)
    }

    test ("multiple consistent parameterised state updates are allowed") {
        val p = new M.ParamState[String,Int] ("p")
        M.reset
        p ("one") := 0
        p ("one") := 0
        M.performUpdates
        expectResult (true) (p ("one") =:= 0)
        expectResult (false) (p ("one") =:= 1)
    }

    test ("inconsistent parameterised state updates in differents steps are allowed") {
        val p = new M.ParamState[String,Int] ("p")
        M.reset
        p ("one") := 0
        M.performUpdates
        expectResult (true) (p ("one") =:= 0)
        expectResult (false) (p ("one") =:= 1)
        M.reset
        p ("one"):= 1
        M.performUpdates
        expectResult (false) (p ("one") =:= 0)
        expectResult (true) (p ("one") =:= 1)
    }

    test ("inconsistent parameterised state updates in one step trigger an exception") {
        val p = new M.ParamState[String,Int] ("p")
        M.reset
        p ("one") := 0
        p ("one") := 1
        val i = intercept[InconsistentUpdateException[Int]] {
                    M.performUpdates
                }
        expectResult ("Machine = M, update = M.p(one) := 0, other value = 1") (i.getMessage)
    }

    // Tests of step debugging trace

    val mmemitter = new StringEmitter

    object MM extends Machine ("MM", mmemitter) {
        override val debug = true

        val s = new State[Int] ("s")
        val t = new State[String] ("t")
        val p = new ParamState[String,Int] ("p")

        def main {
            if (s.isUndefined) {
                s := 0
                p ("one") := 42
                p ("two") := 99
            } else if (s =:= 0) {
                s := 1
                t := "hello"
                p ("two") := 88
                p ("three") := 66
            }
        }
    }

    test ("running multiple steps produces a suitable trace") {
        MM.run
        expectResult ("""MM step 0
                  |MM.p(two) := 99
                  |MM.p(one) := 42
                  |MM.s := 0
                  |MM step 1
                  |MM.p(three) := 66
                  |MM.p(two) := 88
                  |MM.t := hello
                  |MM.s := 1
                  |MM step 2
                  |""".stripMargin) (mmemitter.result)
    }

}
