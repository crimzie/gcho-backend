name := """gcho-backend"""
version := "0.3.5"
scalaVersion := "2.12.4"

fork in run := true
logLevel := Level.Info

lazy val root = project in file(".") enablePlugins PlayScala

libraryDependencies ++= Seq(
  ehcache,
  filters,
  "com.outr" %% "scribe" % "1.4.3",
  "com.typesafe.play" %% "play-mailer" % "6.0.0",
  "com.mohiva" %% "play-silhouette" % "5.0.0",
  "com.mohiva" %% "play-silhouette-testkit" % "5.0.0" % "test",
  "com.mohiva" %% "play-silhouette-password-bcrypt" % "5.0.0",
  "com.mohiva" %% "play-silhouette-persistence" % "5.0.0",
  "com.mohiva" %% "play-silhouette-crypto-jca" % "5.0.0",
  "com.iheart" %% "ficus" % "1.4.0",
  "org.reactivemongo" %% "reactivemongo" % "0.12.6",
  "org.reactivemongo" %% "reactivemongo-play-json" % "0.12.6-play26",
  "io.swagger" %% "swagger-play2" % "1.6.0",
  "org.webjars" %% "webjars-play" % "2.6.2",
  "org.webjars" % "swagger-ui" % "3.1.7",
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
