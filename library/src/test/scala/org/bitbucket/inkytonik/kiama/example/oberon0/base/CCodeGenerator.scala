/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package base

/**
 * C Code generator for the base language.
 */
trait CCodeGenerator extends Translator {

    import c.{
        CArrayType,
        CBlock,
        CDeclaration,
        CEmptyStmt,
        CFunctionDecl,
        CIntExp,
        CIntType,
        CProgram,
        CReturn,
        CStatement,
        CStrType,
        CVarDecl
    }
    import source.{Block, Declaration, EmptyStmt, ModuleDecl, Statement}

    /**
     * Generate C equivalent of a module.
     */
    def translate(m : ModuleDecl) : CProgram = {
        val ModuleDecl(_, Block(ds, ss), _) = m
        val main =
            CFunctionDecl(
                CVarDecl("main", CIntType()),
                Vector(
                    CVarDecl("argc", CIntType()),
                    CVarDecl("argv", CArrayType(0, CStrType()))
                ),
                CBlock(
                    Vector(),
                    (ss map translate) :+
                        CReturn(CIntExp(0))
                )
            )
        CProgram(Vector(), (ds map translate).flatten :+ main)
    }

    /**
     * Interface to C translation of declarations.
     */
    def translate(d : Declaration) : Vector[CDeclaration]

    /**
     * Generate C equivalent of a statement.
     */
    def translate(s : Statement) : CStatement =
        s match {
            case EmptyStmt() =>
                CEmptyStmt()
            case Block(ds, ss) =>
                CBlock((ds map translate).flatten, ss map translate)
        }

}
