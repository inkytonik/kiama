package kiama.example.picojava

object Main {
  
    import java.io.FileReader
  
    def main (args : Array[String]) : Unit = {
        for (filename <- args) {
            val input = new FileReader (filename)
            println (filename)
            val ast = Parser.run (input)
            println (ast)
        }
    }
    
}
