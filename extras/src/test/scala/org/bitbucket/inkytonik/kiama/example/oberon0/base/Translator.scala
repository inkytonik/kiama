/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package base

/**
 * Interface for all translators to C.
 */
trait Translator {

    import c.{CDeclaration, CProgram, CStatement}
    import source.{Declaration, ModuleDecl, Statement}

    /**
     * Generate C equivalent of a module.
     */
    def translate(m : ModuleDecl) : CProgram

    /**
     * Generate C equivalent of a declaration.
     */
    def translate(d : Declaration) : Vector[CDeclaration]

    /**
     * Generate C equivalent of a statement.
     */
    def translate(s : Statement) : CStatement

}
