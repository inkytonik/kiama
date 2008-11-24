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
    
    var time : Long = 0
    var result = 0
    
    for (i <- 0 to 100) {
        // Create a big AST
        
        var bigAsst = createAst(basicAst)
        for (i <- 0 to 150) bigAsst = createAst(bigAsst)
        
        // Evaluate some attributes
        val start = System.currentTimeMillis
        result = (createProgram(bigAsst)->errors).size
        time += (System.currentTimeMillis - start)
        
        
        Attribution.resetMemo
    }
    println("Found " + result + " errors in each run; this took " + time + "ms")
}
