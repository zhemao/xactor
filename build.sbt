organization := "edu.berkeley.eecs"

version := "0.1-SNAPSHOT"

name := "xactor"

scalaVersion := "2.10.4"

addSbtPlugin("com.github.scct" % "sbt-scct" % "0.2")

libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT"

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.0"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
