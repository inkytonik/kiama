/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

/**
 * An attribution module. Use an instance of this module to encapuslate
 * related attributes. You should ensure that more than one circular
 * attribute evaluation from a single module is not executing at the
 * same time because the current implementation has shared state between
 * related circular attributes. If your attributes are unrelated (i.e.,
 * can't possibly call each other) you should base them on different
 * attribution module instances and then it is safe for attributes from
 * different collections to execute in parallel.
 */
class Attribution extends AttributionCore
