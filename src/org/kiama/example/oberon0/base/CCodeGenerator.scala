package org.kiama
package example.oberon0
package base

/**
 * C Code generator for the base language.
 */
trait CCodeGenerator extends Translator {
    
    import c.{CArrayType, CBlock, CDeclaration, CEmptyStmt, CFunctionDecl,
        CIntExp, CIntType, CProgram, CReturn, CStatement, CStrType, CVarDecl}
    import source.{Block, Declaration, EmptyStmt, ModuleDecl, Statement}

    /**
     * Generate C equivalent of a module.
     */
    def translate (m : ModuleDecl) : CProgram = {
        val ModuleDecl (_, Block (ds, ss), _) = m
        val main = 
            CFunctionDecl (CVarDecl ("main", CIntType ()),
                           List (CVarDecl ("argc", CIntType ()),
                                 CVarDecl ("argv", CArrayType (0, CStrType ()))),
                           CBlock (Nil,
                                   (ss map translate) :+
                                        CReturn (CIntExp (0))))
        CProgram (Nil, (ds map translate).flatten ++ List (main))
    }

    /**
     * Interface to C translation of declarations.
     */
    def translate (d : Declaration) : List[CDeclaration]

    /**
     * Generate C equivalent of a statement.
     */
    def translate (s : Statement) : CStatement =
        s match {
            case EmptyStmt ()   =>
                CEmptyStmt ()
            case Block (ds, ss) =>
                CBlock ((ds map translate).flatten, ss map translate)
        }

}
