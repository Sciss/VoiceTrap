package de.sciss.voicetrap
package impl

import de.sciss.lucre.{DataInput, DataOutput, stm}
import de.sciss.synth.proc.{AuralSystem, Transport}

object ChannelImpl {
   private final val SER_VERSION = 1

   implicit object serializer extends stm.Serializer[ Tx, Acc, Channel ] {
      def write( v: Channel, out: DataOutput ) { v.write( out )}

      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Channel = {
         readSerVersion( in, SER_VERSION )
         val row     = in.readInt()
         val column  = in.readInt()
         new Impl( row, column )
      }
   }

   def apply( row: Int, column: Int )( implicit tx: Tx ) : Channel = {
      new Impl( row, column )
   }

   private final class Impl( val row: Int, val column: Int ) extends Channel {
      override def toString = "Channel(row=" + row + ", column=" + column + ")"

      def hiddenLayer : AudioArtifact = ???

      def cursor( implicit tx: Tx ) : Cursor = ???

      def start( auralSystem: AuralSystem[ S ])( implicit tx: Tx ) {
         ???
      }

      def stop()( implicit tx: Tx ) { ??? }

      def fork()( implicit tx: Tx ) { ??? }

      def write( out: DataOutput ) {
         writeSerVersion( out, SER_VERSION )
         out.writeInt( row )
         out.writeInt( column )
      }
   }
}