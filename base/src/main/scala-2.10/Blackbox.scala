/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package scala

/**
 * Make 2.10 macro contexts available under a "blackbox" prefix.
 * See https://issues.scala-lang.org/browse/SI-8209.
 */
object blackbox {
    type Context = scala.reflect.macros.Context
}
