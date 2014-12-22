/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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
