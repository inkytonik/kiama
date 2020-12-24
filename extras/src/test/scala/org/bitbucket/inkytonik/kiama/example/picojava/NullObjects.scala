/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
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

trait NullObjects {

    self : Attribution with NameResolution with TypeAnalyser with PredefinedTypes =>

    import PicoJavaTree._

    /**
     * A declaration object representing an unknown entity.
     *
     * syn lazy UnknownDecl Program.unknownDecl() = (UnknownDecl) localLookup("\$unknown");
     * inh Decl TypeDecl.unknownDecl();
     * inh Decl Block.unknownDecl();
     * eq Program.getBlock().unknownDecl() = unknownDecl();
     * eq Program.getPredefinedType().unknownDecl() = unknownDecl();
     */
    val unknownDecl : PicoJavaNode => UnknownDecl =
        attr {
            case p : Program =>
                localLookup("$unknown")(p).asInstanceOf[UnknownDecl]
            // FIXME: need NTA case?
            case tree.parent(p) =>
                unknownDecl(p)
            case n =>
                sys.error(s"unknownDecl: unexpected PicoJavaNode $n")
        }

}
