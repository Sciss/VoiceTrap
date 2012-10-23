package de.sciss.voicetrap

import impl.{ChannelImpl => Impl}
import de.sciss.lucre.Writable

object Channel {
   implicit def serializer: Serializer[ Channel ] = Impl.serializer

   def apply( row: Int, column: Int )( implicit tx: Tx ) : Channel = Impl( row, column )
}
trait Channel extends Writable {
   def row: Int
   def column: Int

   def hiddenLayer : AudioArtifact
   def cursor( implicit tx: Tx ): Cursor

   // ---- algorithm ----

   def start() : Unit
   def stop() : Unit

   def fork()( implicit tx: Tx ) : Unit
}