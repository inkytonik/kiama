package kiama.example.picojava.benchmark

import kiama.attribution._
import kiama.example.picojava.tests._
import kiama.example.picojava.AbstractSyntax._
import kiama.example.picojava.ErrorCheck._

object PicoJavaBenchmark extends Application {
    // For the actual program text this is based on, see DotNameResolutionTests.pj
    
    def basicAst = ClassDecl ("AA", None, Block (List (VarDecl ("x", Use ("int")))))
    
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
    
    for (i <- 0 to 10000) {
        val result = createProgram(createAst(basicAst))->errors
        Attribution.resetMemo
    }
    
    /* Two-step benchmark
    
    // Initialize inputs
    
    val inputs = new scala.collection.mutable.ArrayBuffer[Program]
    for (i <- 0 to 100) {
        var bigAsst = createAst(basicAst)
        for (i <- 0 to 150) bigAsst = createAst(bigAsst)
        inputs += createProgram(bigAsst)
    }

    // Evaluate some attributes

    var result = 0
    val start = System.currentTimeMillis
    
    for (p <- inputs) {
        result = (p->errors).size
        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }
    
    println("Found " + result + " errors in each run; this took a total of " + (System.currentTimeMillis - start) + "ms")
    */

    var time : Long = 0
    var result : Int = 0
    
    for (i <- 0 to 100) {
        var bigAsst = createAst(basicAst)
        for (j <- 0 to 150) bigAsst = createAst(bigAsst)
        val p = createProgram(bigAsst)
        
        val start = System.nanoTime
        result = (p->errors).size
        time += (System.nanoTime - start)
        
        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }
    
    System.out.println("Found " + result + " errors in each run; this took " + (time / 1000000) + "ms")
}
