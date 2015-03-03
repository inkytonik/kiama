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
 * Interface for all translators to C.
 */
trait Translator {

    import c.{CDeclaration, CProgram, CStatement}
    import source.{Declaration, ModuleDecl, Statement}
    import source.SourceTree.SourceTree

    /**
     * Generate C equivalent of a module.
     */
    def translate (m : ModuleDecl) : CProgram

    /**
     * Generate C equivalent of a declaration.
     */
    def translate (d : Declaration) : List[CDeclaration]

    /**
     * Generate C equivalent of a statement.
     */
    def translate (s : Statement) : CStatement

}
