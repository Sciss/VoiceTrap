package de.sciss.voicetrap

object Channel {
   implicit def serializer: Serializer[ Channel ] = ???
}
trait Channel {
   def row: Int
   def column: Int

   def hiddenLayer : AudioArtifact
   def cursor( implicit tx: Tx ): Cursor
}