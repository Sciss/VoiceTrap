package de.sciss.voicetrap

import de.sciss.lucre.stm
import de.sciss.synth.proc.Grapheme

object Channel {
   implicit def serializer: stm.Serializer[ Tx, Acc, Channel ] = ???
}
trait Channel {
   def row: Int
   def column: Int

   def hiddenLayer : Grapheme.Value.Audio
   def cursor( implicit tx: Tx ): Cursor
}