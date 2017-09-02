scalacOptions += "-Xexperimental" // Enable SAM type for Scala 2.11.11

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

scalacOptions += "-Ydelambdafy:inline"

enablePlugins(Optimization)
