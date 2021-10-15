name := "fs2-workshop"

scalaVersion := "2.13.6" // Can be decreased down to 2.12 if you want to

val FS2Version = "3.1.1"
val CatsEffectVersion = "3.2.9"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % FS2Version,
  "co.fs2" %% "fs2-io" % FS2Version,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion
)