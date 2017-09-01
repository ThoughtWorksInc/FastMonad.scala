enablePlugins(JmhPlugin)

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.15"

scalacOptions += "-Xexperimental" // Enable SAM type for Scala 2.11.11
//
//scalacOptions += "-optimise"
//
//scalacOptions += "-Ydelambdafy:inline"