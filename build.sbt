lazy val fastmonad = project

lazy val benchmark = project.dependsOn(fastmonad)

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.3")
