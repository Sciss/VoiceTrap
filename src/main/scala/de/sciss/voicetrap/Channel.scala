package de.sciss.voicetrap

import impl.{ChannelImpl => Impl}
import de.sciss.lucre.Writable
import de.sciss.synth.proc.AuralSystem

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

   def start( auralSystem: AuralSystem[ S ])( implicit tx: Tx ) : Unit
   def stop()( implicit tx: Tx ) : Unit

   def fork()( implicit tx: Tx ) : Unit
}