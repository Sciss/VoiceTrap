package de.sciss.voicetrap
package impl

import de.sciss.lucre.{stm, DataOutput, DataInput}
import de.sciss.synth.proc.{ArtifactStore, AuralSystem, ProcGroup_}
import java.io.File

object DocumentImpl {
   private final val SER_VERSION = 1

   implicit object serializer extends stm.Serializer[ Tx, Acc, Document ] {
      def write( v: Document, out: DataOutput ) { v.write( out )}
      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Document = {
         readSerVersion( in, SER_VERSION )
         val cursor        = tx.readCursor( in, access )
         val group         = ProcGroup_.Modifiable.read[ S ]( in, access )
         val channels      = mapSerializer[ (Int, Int), Channel ].read( in, access )
         implicit val artifactStore = ArtifactStore.read[ S ]( in, access )
         new Impl( cursor, group, channels )
      }
   }

   def apply()( implicit tx: Tx ) : Document = {
      val cursor     = tx.newCursor()
      val group      = ProcGroup_.Modifiable[ S ]
      val channels   = for( row <- 0 until VoiceTrap.numRows; column <- 0 until VoiceTrap.numColumns ) yield {
         (row, column) -> Channel( row, column )
      }
      implicit val artifactStore = ArtifactStore[ S ]( new File( VoiceTrap.baseDirectory, "artifacts" ))
      new Impl( cursor, group, channels.toMap )
   }

   private final class Impl( val cursor: Cursor, val group: ProcGroup, val channels: Map[ (Int, Int), Channel ])
                           ( implicit val artifactStore : ArtifactStore[ S ])
   extends Document {
      doc =>

      override def toString = "Document"

      /**
       * Fork random range bounds in seconds
       */
      val minMaxFork = (60.0, 240.0)

      /**
       * Wrapping duration in seconds for the performance time
       */
      val pDur = 60.0

      def start( auralSystem: AuralSystem[ S ])( implicit tx: Tx ) {
         channels.valuesIterator.foreach( _.start( doc, auralSystem ))
      }

      def stop()( implicit tx: Tx ) {
         channels.valuesIterator.foreach( _.stop() )
      }

      def write( out: DataOutput ) {
         writeSerVersion( out, SER_VERSION )
         cursor.write( out )
         group.write( out )
         mapSerializer[ (Int, Int), Channel ].write( channels, out )
         artifactStore.write( out )
      }
   }
}