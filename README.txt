Chisel Transactors
=====================

This project implements a transactional API layer on top of the Chisel
hardware construction language. Our library allows you to express hardware
as actors, which contain input queues, output queues, internal state, and
guarded atomic actions. You can find examples of how to write an actor
under src/test/scala. To use transactors in your hardware design, first
install this library using "sbt publish-local". Then, add the following line
to your project's build.sbt file.

    libraryDependencies += "edu.berkeley.eecs" %% "xactor" % "0.1-SNAPSHOT"
