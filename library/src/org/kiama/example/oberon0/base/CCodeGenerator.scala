/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

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
