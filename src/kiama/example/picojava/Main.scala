package kiama.example.picojava

object Main {
  
    import java.io.FileReader
    import ErrorCheck._
  
    def main (args : Array[String]) : Unit = {
        for (filename <- args) {
            val program = Parser.run (new FileReader (filename))
            val messages = program->errors
            for (msg <- messages)
                println (filename + ":" + msg)
        }
    }
    
}
