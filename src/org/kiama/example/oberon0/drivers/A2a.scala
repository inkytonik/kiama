package org.kiama
package example.oberon0
package drivers

trait A2aPhases extends base.Driver
    with L3.Parser
    with L3.source.PrettyPrinter
    with L3.NameAnalyser {

    def artefact = "A2a"
    def langlevel = 3
    def tasklevel = 2

}

object A2a extends A2aPhases
