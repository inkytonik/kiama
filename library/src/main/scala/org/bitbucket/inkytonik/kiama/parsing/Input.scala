/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2016 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package parsing

import org.bitbucket.inkytonik.kiama.util.Source

/**
 * The input consumed by a parser.
 */
case class Input(source : Source, offset : Int) {

    import org.bitbucket.inkytonik.kiama.util.Position

    /**
     * Are we at the end of the input?
     */
    val atEnd : Boolean =
        offset == source.content.length

    /**
     * The first character of the input if we are not at the end.
     */
    val first : Option[Char] =
        if (atEnd)
            None
        else
            Some(source.content.charAt(offset))

    /**
     * Return a description of the current character found in the input,
     * either the actual character is there is one, or "end of source" if
     * we are at the end.
     */
    val found : String =
        if (offset == source.content.length)
            "end of source"
        else
            s"'${source.content.charAt(offset)}'"

    /**
     * Return the current position of the input.
     */
    val position : Position =
        source.offsetToPosition(offset)

    /**
     * The rest of the input, unchanged if already at end.
     */
    def rest : Input =
        if (atEnd)
            this
        else
            Input(source, offset + 1)

}
