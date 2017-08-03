name := """ggmtools"""

version := "0.2"

lazy val commonSettings = Seq(scalaVersion := "2.11.8")

lazy val ggmtools = project in file(".") enablePlugins PlayScala settings (commonSettings: _*)

lazy val preloader = project dependsOn ggmtools settings (commonSettings: _*)

routesGenerator := InjectedRoutesGenerator

libraryDependencies ++= Seq(
  filters,
  jdbc,
  cache,
  ws,
  "com.typesafe.play" %% "play-mailer" % "5.0.0",
  "com.mohiva" %% "play-silhouette" % "4.0.0",
  "com.mohiva" %% "play-silhouette-testkit" % "4.0.0" % "test",
  "com.mohiva" %% "play-silhouette-password-bcrypt" % "4.0.0",
  "com.mohiva" %% "play-silhouette-persistence" % "4.0.0",
  "com.mohiva" %% "play-silhouette-crypto-jca" % "4.0.0",
  "com.iheart" %% "ficus" % "1.4.0",
  "org.webjars" %% "webjars-play" % "2.4.0",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.12.1",
  "net.codingwell" %% "scala-guice" % "4.0.0",
  "org.webjars" % "swagger-ui" % "2.1.2",
  "org.scalaz" %% "scalaz-core" % "7.2.6",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0" % "test",
  "org.mockito" % "mockito-core" % "1.10.19",
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.7")

resolvers ++= Seq(
  "Atlassian Releases" at "https://maven.atlassian.com/public/",
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  Resolver.sonatypeRepo("snapshots")
)

unmanagedResourceDirectories in Test <+= baseDirectory(_ / "target/web/public/test")

fork in run := true
