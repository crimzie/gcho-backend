name := """gcho-backend"""
version := "0.2"
scalaVersion := "2.12.3"
fork in run := true
routesGenerator := InjectedRoutesGenerator
unmanagedResourceDirectories in Test <+= baseDirectory(_ / "target/web/public/test")
logLevel := sbt.Level.Info

lazy val root = project in file(".") enablePlugins PlayScala

libraryDependencies ++= Seq(
  ehcache,
  filters,
  guice,
  ws,
  "com.typesafe.play" %% "play-mailer" % "6.0.0",
  "com.mohiva" %% "play-silhouette" % "5.0.0",
  "com.mohiva" %% "play-silhouette-testkit" % "5.0.0" % "test",
  "com.mohiva" %% "play-silhouette-password-bcrypt" % "5.0.0",
  "com.mohiva" %% "play-silhouette-persistence" % "5.0.0",
  "com.mohiva" %% "play-silhouette-crypto-jca" % "5.0.0",
  "com.iheart" %% "ficus" % "1.4.0",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.12.6-play26",
  "net.codingwell" %% "scala-guice" % "4.1.0",
  "org.webjars" % "swagger-ui" % "3.1.4",
  "org.scalaz" %% "scalaz-core" % "7.2.15",
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.1" % "test",
  "org.mockito" % "mockito-core" % "1.10.19" % "test",
  "com.sksamuel.scrimage" %% "scrimage-core" % "3.0.0-alpha3",
  "com.github.dwickern" %% "scala-nameof" % "1.0.3" % "provided")

resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Atlassian Releases" at "https://maven.atlassian.com/public/",
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  Resolver sonatypeRepo "snapshots")
