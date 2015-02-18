scalaVersion := "2.11.4"

EclipseKeys.withSource := true

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"


//scalaSource in Compile := baseDirectory.value / "src"

unmanagedSourceDirectories in Compile := Nil

unmanagedSourceDirectories in Compile += baseDirectory.value / "src"

unmanagedSourceDirectories in Compile += baseDirectory.value / "Scala-Common" / "src"

//scalaTest in Compile := baseDirectory.value / "test"

unmanagedSourceDirectories in Test <<= (sourceDirectory){ src => src / "test" :: Nil}

sourcesInBase := false



