package org.kiama
package example.oberon0
package base

/**
 * Interface for all translaters to C.
 */
trait Translator {

    import c.{CDeclaration, CProgram, CStatement}
    import source.{Declaration, ModuleDecl, Statement}

    /**
     * Generate C equivalent of a module.
     */
    def translate (m : ModuleDecl) : CProgram

    /**
     * Generate C equivalent of a declaration.
     */
    def translate (d : Declaration) : List[CDeclaration]

    /**
     * Generate C equivalents of statements.
     */
    def translate (s : Statement) : CStatement

}
