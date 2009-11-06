/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
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

/**
 * sbt project configuration for kiama
 */
class KiamaProject (info: ProjectInfo) extends DefaultProject (info)
{
    // Configure paths
    override def mainScalaSourcePath = "src"
    override def testScalaSourcePath = "src"
    override def outputDirectoryName = "bin"
    override def mainCompilePath     = outputDirectoryName
    override def testCompilePath     = outputDirectoryName
    
    // Specyify how to find source and test files.  Sources are
    //    - all .scala files, except
    //    - files whose names end in Tests.scala, which are tests
    def mainSourceFilter = "*.scala" && -testSourceFilter
    def testSourceFilter = "*Tests.scala" 
    override def mainSources = descendents (mainSourceRoots, mainSourceFilter)
    override def testSources = descendents (testSourceRoots, testSourceFilter)
    
    // There is no main class since this is a library
    override def getMainClass (promptIfMultipleChoices : Boolean) = None

    // Set compiler options
    override def compileOptions = super.compileOptions ++ Seq (Unchecked)

    // Declare dependencies on other libraries
    override def libraryDependencies =
        Set ("org.scala-tools.testing" % "scalacheck" % "1.6",
             "org.scalatest" % "scalatest" % "1.0",
             "junit" % "junit" % "4.7",
             "jline" % "jline" % "0.9.94")

    // Remove LinkSource from doc options since it doesn't appear to work
    override def documentOptions: Seq [ScaladocOption] =
        documentTitle (name + " " + version + " API") ::
        windowTitle (name + " " + version + " API") ::
        Nil
    
    // Add extra files to included resources
    def extraResources = "COPYING" +++ "COPYING.LESSER" +++ "COPYING.OTHER" +++ "README.txt"
    override def mainResources = super.mainResources +++ extraResources
    
    // By default, only log warnings or worse
    log.setLevel (Level.Warn)

    // Publish to Maven style repo at scala-tools.org
    override def managedStyle = ManagedStyle.Maven
    val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"

    // Get credentials from here
    Credentials (Path.userHome / ".ivy2" / ".credentials", log)

    // Publish sources and scaladocs too
    override def packageDocsJar = defaultJarPath ("-scaladoc.jar")
    override def packageSrcJar= defaultJarPath ("-sources.jar")
    val sourceArtifact = Artifact (artifactID, "src", "jar", Some ("sources"), Nil, None)
    val docsArtifact = Artifact (artifactID, "docs", "jar", Some ("scaladoc"), Nil, None)
    override def packageToPublishActions = super.packageToPublishActions ++ Seq (packageDocs, packageSrc)    
}
