resolvers += Resolver.url ("scalasbt",
    new URL ("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")
) (Resolver.ivyStylePatterns)

// * xsbt-gpg-plugin, required for publishing
 
addSbtPlugin ("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")

// * sbt-mima-plugin, use for checking migration, binary compat

addSbtPlugin ("com.typesafe" % "sbt-mima-plugin" % "0.1.3")

// * findbugs4sbt, optional

// addSbtPlugin ("de.johoop" % "findbugs4sbt" % "1.1.2")
