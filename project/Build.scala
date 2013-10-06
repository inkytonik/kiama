/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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
import sbt.Keys._

object KiamaBuild extends Build {

    import sbt.Project.Initialize
    import sbtunidoc.Plugin.{unidocSettings, ScalaUnidoc, TestScalaUnidoc}
    import sbtunidoc.Plugin.Unidoc
    import sbtunidoc.Plugin.UnidocKeys.unidoc

    // Project configuration:
    //   - core project containing macros and code that they need
    //   - library project containing everything else, including all tests
    //   - kiama (root) project aggregates core and library

    lazy val kiama =
        Project (
            id = "kiama",
            base = file (".")
        ) aggregate (core, library) settings (
            allNewSettings : _*
        )

    lazy val core =
        Project (
            id = "core",
            base = file ("core")
        )

    lazy val library =
        Project (
            id = "library",
            base = file ("library")
        ) dependsOn (core % "compile-internal, test-internal")

    // All settings that have to be added to the kiama project
    lazy val allNewSettings : Seq[Setting[_]] =
        Seq (
            mappings in (Compile, packageBin) :=
                (mappings in (core, Compile, packageBin)).value ++
                    (mappings in (library, Compile, packageBin)).value,
            mappings in (Compile, packageSrc) :=
                (mappings in (core, Compile, packageSrc)).value ++
                    (mappings in (library, Compile, packageSrc)).value,
            mappings in (Test, packageBin) :=
                (mappings in (library, Test, packageBin)).value,
            mappings in (Test, packageSrc) :=
                (mappings in (library, Test, packageSrc)).value
        ) ++
        unidocSettings ++
        Seq (
            doc in Compile := (doc in ScalaUnidoc).value,
            doc in Test := (doc in TestScalaUnidoc).value,
            target in unidoc in ScalaUnidoc := crossTarget.value / "api",
            target in unidoc in TestScalaUnidoc := crossTarget.value / "test-api"
        )

}
