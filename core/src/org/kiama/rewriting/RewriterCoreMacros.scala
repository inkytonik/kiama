/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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
package rewriting

object RewriterCoreMacros {

    import org.bitbucket.inkytonik.dsname.DSName.makeCallWithName
    import org.kiama.util.Emitter
    import scala.reflect.macros.Context

    // Macros for the builder methods

    def allMacro (c : Context) (s : c.Expr[Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"all")

    def buildMacro (c : Context) (t : c.Expr[Any]) : c.Expr[Strategy] =
        makeCallWithName (c, s"build")

    def childMacro (c : Context) (i : c.Expr[Int], s : c.Expr[Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"child")

    def condMacro (c : Context) (lr : c.Expr[PlusStrategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"this.<")

    def congruenceMacro (c : Context) (ss : c.Expr[Strategy]*) : c.Expr[Strategy] =
        makeCallWithName (c, s"congruence")

    def debugMacro (c : Context) (msg : c.Expr[String], emitter : c.Expr[Emitter]) : c.Expr[Strategy] =
        makeCallWithName (c, s"debug")

    def detchoiceMacro (c : Context) (q : c.Expr[Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"this.<+")

    def logMacro (c : Context) (s : c.Expr[Strategy], msg : c.Expr[String], emitter : c.Expr[Emitter]) : c.Expr[Strategy] =
        makeCallWithName (c, s"log")

    def logfailMacro (c : Context) (s : c.Expr[Strategy], msg : c.Expr[String], emitter : c.Expr[Emitter]) : c.Expr[Strategy] =
        makeCallWithName (c, s"logfail")

    def memoMacro (c : Context) (s : c.Expr[Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"memo")

    def nondetchoiceMacro (c : Context) (q : c.Expr[Strategy]) : c.Expr[PlusStrategy] =
        makeCallWithName (c, s"this.+")

    def oneMacro (c : Context) (s : c.Expr[Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"one")

    def optionMacro (c : Context) (o : c.Expr[Option[Any]]) : c.Expr[Strategy] =
        makeCallWithName (c, s"option")

    def queryMacro[T] (c : Context) (f : c.Expr[Any ==> T]) : c.Expr[Strategy] =
        makeCallWithName (c, s"query")

    def queryfMacro[T] (c : Context) (f : c.Expr[Any => T]) : c.Expr[Strategy] =
        makeCallWithName (c, s"queryf")

    def ruleMacro (c : Context) (f : c.Expr[Any ==> Any]) : c.Expr[Strategy] =
        makeCallWithName (c, s"rule")

    def rulefMacro (c : Context) (f : c.Expr[Any => Any]) : c.Expr[Strategy] =
        makeCallWithName (c, s"rulef")

    def rulefsMacro (c : Context) (f : c.Expr[Any ==> Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"rulefs")

    def seqMacro (c : Context) (q : c.Expr[Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"this.<*")

    def someMacro (c : Context) (s : c.Expr[Strategy]) : c.Expr[Strategy] =
        makeCallWithName (c, s"some")

    def strategyMacro (c : Context) (f : c.Expr[Any ==> Option[Any]]) : c.Expr[Strategy] =
        makeCallWithName (c, s"strategy")

    def strategyfMacro (c : Context) (f : c.Expr[Any => Option[Any]]) : c.Expr[Strategy] =
        makeCallWithName (c, s"strategyf")

    def termMacro (c : Context) (t : Any) : c.Expr[Strategy] =
        makeCallWithName (c, s"term")

}
