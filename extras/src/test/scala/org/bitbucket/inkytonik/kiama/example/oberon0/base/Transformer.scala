/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package base

/**
 * Interface for all transformers. Also provides operations that are
 * useful in transformer implementations.
 */
trait Transformer {

    import source.SourceTree.SourceTree

    /**
     * Transform a module tree in some way, returning a new module tree.
     * By default, just return the given module.
     */
    def transform(tree : SourceTree) : SourceTree =
        tree

}
