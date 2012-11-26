package org.kiama
package example.oberon0
package drivers

trait A4Phases extends base.TranslatingDriver
    with L4.Parser
    with L4.source.PrettyPrinter
    with L4.NameAnalyser
    with L4.TypeAnalyser
    with L2.Lifter
    with L2.Desugarer
    with L4.CCodeGenerator
    with L4.c.PrettyPrinter {

    def artefact : String = "A4"
    def langlevel : Int = 4
    def tasklevel : Int = 6

}

object A4 extends A4Phases
