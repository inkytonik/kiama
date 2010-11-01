/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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
package example.picojava.benchmark

object PicoJavaBenchmark extends Application {

    import org.kiama.attribution._
    import org.kiama.example.picojava.AbstractSyntax._
    import org.kiama.example.picojava.ErrorCheck._

    // For the actual program text this is based on, see DotNameResolutionTests.pj

    def basicAst () = ClassDecl ("AA", None, Block (List (VarDecl ("x", Use ("int")))))

    def createAst (subtree : ClassDecl) =
        ClassDecl ("AA", None, Block (
              List (VarDecl ("y", Use ("int")),
                    VarDecl ("a", Use ("AA")),
                    AssignStmt (Use ("x"),
                                Dot (Use ("a"), Use ("x"))),
                    subtree,
                    ClassDecl ("BB", Some (Use ("AA")), Block (
                        List (VarDecl ("b", Use ("BB")),
                              AssignStmt (Dot (Use ("b"), Use("y")),
                                          Dot (Use ("b"), Use("x")))))))))

    def createProgram (subtree : ClassDecl) =
        Program (Block (List (subtree)))

    // Warm up the JIT compiler

    for (i <- 0 until 15000) {
        val result = createProgram(createAst(createAst(basicAst)))->errors
        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }

    System.gc

    // Two-step benchmark

    // Initialize inputs

    val inputs = new scala.collection.mutable.ArrayBuffer[Program]
    for (i <- 0 until 100) {
        var bigAsst = createAst(basicAst)
        for (i <- 0 until 150) bigAsst = createAst(bigAsst)
        inputs += createProgram(bigAsst)
    }

    // Evaluate some attributes

    var result = 0
    val start = System.currentTimeMillis

    for (i <- 0 until inputs.size) {
        val p = inputs(i)
        inputs(i) = null
        result = (p->errors).size
        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }

    println((System.currentTimeMillis - start))

  /*
    var time : Long = 0
    var result : Int = 0

    for (i <- 0 until 100) {
        var bigAsst = createAst(basicAst)
        for (j <- 0 until 150) bigAsst = createAst(bigAsst)
        val p = createProgram(bigAsst)

        val start = System.nanoTime
        result = (p->errors).size
        time += (System.nanoTime - start)

        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }

    println((time / 1000000))
    */
}
