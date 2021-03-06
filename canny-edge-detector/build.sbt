lazy val commonSettings = Seq(
    organization := "io.github.arnodb",
    version := "0.1.0",
    scalaVersion := "2.11.6"
)

lazy val root = (project in file(".")).
    settings(commonSettings: _*).
    settings(
        name := "canny-edge-detector",
        libraryDependencies ++= Seq(
            "org.specs2" %% "specs2-core" % "3.6.4" % "test"
        )
    )

