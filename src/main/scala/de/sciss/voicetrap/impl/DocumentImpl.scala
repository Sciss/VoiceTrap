package de.sciss.voicetrap
package impl

import de.sciss.lucre.{stm, DataOutput, DataInput}
import de.sciss.synth.proc.ProcGroup_

object DocumentImpl {
   implicit object serializer extends stm.Serializer[ Tx, Acc, Document ] {
      def write( v: Document, out: DataOutput ) { v.write( out )}
      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Document = {
         val group = ProcGroup_.Modifiable.read[ S ]( in, access )
         new Impl( group )
      }
   }

   def apply()( implicit tx: Tx ) : Document = {
      val group = ProcGroup_.Modifiable[ S ]
      new Impl( group )
   }

   private final class Impl( val group: ProcGroup ) extends Document {
      /**
       * Map from (row, column) to channel
       */
      def channels : Map[ (Int, Int), Channel ] = ???

      /**
       * Fork random range bounds in seconds
       */
      val minMaxFork = (60.0, 240.0)

      /**
       * Wrapping duration in seconds for the performance time
       */
      val pDur = 60.0


      def start() {
         channels.valuesIterator.foreach( _.start() )
      }

      def stop() {
         channels.valuesIterator.foreach( _.stop() )
      }

      def write( out: DataOutput ) {
         group.write( out )
      }
   }
}