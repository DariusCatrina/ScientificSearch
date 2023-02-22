name := "frontend"

resolvers += ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release").withAllowInsecureProtocol(
  true
)

libraryDependencies += guice
libraryDependencies += ehcache
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test
libraryDependencies += "org.postgresql"          % "postgresql"         % "9.3-1102-jdbc41"

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.cs.duke.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.cs.duke.binders._"
