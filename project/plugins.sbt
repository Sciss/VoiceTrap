resolvers ++= Seq(
   "less is" at "http://repo.lessis.me",
   "coda" at "http://repo.codahale.com"
)

addSbtPlugin( "com.eed3si9n" % "sbt-assembly" % "0.8.5" )

// addSbtPlugin( "de.sciss" % "sbt-appbundle" % "0.15" )
