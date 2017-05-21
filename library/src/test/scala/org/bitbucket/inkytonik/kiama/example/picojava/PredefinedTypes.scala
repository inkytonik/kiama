/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.bitbucket.inkytonik.kiama
package example.picojava

import org.bitbucket.inkytonik.kiama.attribution.Attribution

trait PredefinedTypes {

    self : Attribution with NameResolution =>

    import PicoJavaTree._

    /*
     * A list of declarations of primitive types.
     *
     * syn lazy List Program.getPredefinedTypeList() {
     *    return new List().
     *        add(new UnknownDecl("$unknown")).
     *        add(new PrimitiveDecl("boolean"));
     * }
     */
    val getPredefinedTypeList : Program => Vector[TypeDecl] =
        constant {
            Vector(
                UnknownDecl("$unknown"),
                PrimitiveDecl("boolean"),
                PrimitiveDecl("int")
            )
        }

    /**
     * Make the boolean type available.
     *
     * syn lazy PrimitiveDecl Program.booleanType() = (PrimitiveDecl) localLookup("boolean");
     * inh PrimitiveDecl BooleanLiteral.booleanType();
     * inh PrimitiveDecl WhileStmt.booleanType();
     * inh PrimitiveDecl Decl.booleanType();
     * eq Program.getBlock().booleanType() = booleanType();
     * eq Program.getPredefinedType().booleanType() = booleanType();
     */
    val booleanType : PicoJavaNode => PrimitiveDecl =
        attr {
            case p : Program =>
                localLookup("boolean")(p).asInstanceOf[PrimitiveDecl]
            // FIXME don't have NTA case, needed?
            case tree.parent(p) =>
                booleanType(p)
        }

}
