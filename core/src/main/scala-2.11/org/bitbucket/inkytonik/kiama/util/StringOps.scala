/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

object StringOps {

    import scala.collection.immutable.{StringOps => IStringOps}

    def lines(s : String) : Iterator[String] =
        new IStringOps(s).lines

}
