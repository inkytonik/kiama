/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2011 Anthony M Sloane, Macquarie University.
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

import sbt._

class Plugins (info : ProjectInfo) extends PluginDefinition (info) {
    val snuggletex_repo = "snuggletex_repo" at "http://www2.ph.ed.ac.uk/maven2"    
    val t_repo = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"
    val posterous = "net.databinder" % "posterous-sbt" % "0.1.6"
}
