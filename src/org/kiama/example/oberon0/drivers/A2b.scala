package org.kiama
package example.oberon0
package drivers

trait A2bPhases extends base.Driver
    with L2.Parser
    with L2.source.PrettyPrinter
    with L2.NameAnalyser
    with L2.TypeAnalyser {

    def artefact = "A2b"
    def langlevel = 2
    def tasklevel = 3

}

object A2b extends A2bPhases
