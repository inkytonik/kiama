package org.kiama
package example.oberon0
package drivers

trait A1Phases extends base.Driver
    with L2.Parser
    with L2.source.PrettyPrinter
    with L2.NameAnalyser {

    override def artefact = "A1"
    def langlevel = 2
    def tasklevel = 2
}

object A1 extends A1Phases
