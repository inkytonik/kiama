package kiama

import org.scalacheck._

import kiama.rewriting.RewriterTests

object AllTests extends Properties ("kiama") {
    include (RewriterTests.AllTests)
}

object Tests extends Application {
    Test.checkProperties (AllTests)
}
