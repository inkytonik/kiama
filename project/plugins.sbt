resolvers += Resolver.url ("scalasbt", new URL ("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")) (Resolver.ivyStylePatterns)
 
addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.5")

addSbtPlugin ("de.johoop" % "findbugs4sbt" % "1.1.2")
