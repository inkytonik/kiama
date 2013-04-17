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

    // Project configuration:
    //   - core project containing macros and code that they need
    //   - kiama project containing everything else
    //   - root project aggregates core and kiama
    // Additional tasks:
    //   - kiama/doc and kiama/test:doc generate unified documentation for
    //     both the kiama and core sub-projects

    lazy val root =
        Project (
            id = "root",
            base = file (".")
        ) aggregate (core, kiama)

    lazy val core =
        Project (
            id = "core",
            base = file ("core")
        )

    lazy val kiama =
        Project (
            id = "kiama",
            base = file ("kiama")
        ) dependsOn (core % "compile-internal, test-internal") settings (
            allNewSettings : _*
        )

    // All settings that have to be added to the kiama project
    lazy val allNewSettings : Seq[Setting[_]] =
        Seq (
            mappings in (Compile, packageBin) <++= mappings in (core, Compile, packageBin),
            mappings in (Compile, packageSrc) <++= mappings in (core, Compile, packageSrc)
        ) ++
        allUnidocSettings

    // List of project names whose documentation will be combined in the kiama project
    // unified documentation
    lazy val docProjects = List ("kiama", "core")

    // Override doc to generate unified documentation
    // Based on setup used in Akka, see Akka's project/Unidoc.scala
    // The main difference is that we do not use the aggregation structure to decide
    // what to include, we just include all from docProjects. Also, we redefine the
    // doc task so that things like packaging etc pick up the unified docs not the
    // ones just for the kiama project.

    val unidocDirectory = SettingKey[File] ("unidoc-directory")
    val unidocExclude = SettingKey[Seq[String]] ("unidoc-exclude")
    val unidocAllSources = TaskKey[Seq[Seq[File]]] ("unidoc-all-sources")
    val unidocSources = TaskKey[Seq[File]] ("unidoc-sources")
    val unidocAllClasspaths = TaskKey[Seq[Classpath]] ("unidoc-all-classpaths")
    val unidocClasspath = TaskKey[Seq[File]] ("unidoc-classpath")

    lazy val allUnidocSettings : Seq[Setting[_]] =
        unidocSettings (Compile) ++ unidocSettings (Test)

    def unidocSettings (conf : Configuration) : Seq[Setting[_]] = {
        val subdir = Defaults.prefix (conf.name) + "api"
        Seq (
            doc in conf <<= unidocTask (conf, subdir),
            unidocDirectory in conf <<= crossTarget / subdir,
            unidocAllSources in conf <<= (buildStructure) flatMap allSources (conf),
            unidocSources in conf <<= (unidocAllSources in conf) map { _.flatten },
            unidocAllClasspaths in conf <<= (buildStructure) flatMap allClasspaths (conf),
            unidocClasspath in conf <<= (unidocAllClasspaths in conf) map { _.flatten.map (_.data).distinct }
        )
    }

    def unidocTask (conf : Configuration, subdir : String) : Initialize[Task[File]] =
        (compilers, cacheDirectory, unidocSources in conf, unidocClasspath in conf,
                unidocDirectory in conf, scalacOptions in (conf, doc), streams) map {
            (compilers, cache, sources, classpath, target, options, s) => {
                val scaladoc = new Scaladoc (100, compilers.scalac)
                scaladoc.cached (cache / subdir, "main", sources, classpath, target, options, s.log)
                target
            }
        }

    def allSources (conf : Configuration) (structure : Load.BuildStructure) : Task[Seq[Seq[File]]] = {
        docProjects flatMap { sources in conf in LocalProject (_) get structure.data } join
    }

    def allClasspaths (conf : Configuration) (structure : Load.BuildStructure) : Task[Seq[Classpath]] = {
        docProjects flatMap { dependencyClasspath in conf in LocalProject (_) get structure.data } join
    }

}
