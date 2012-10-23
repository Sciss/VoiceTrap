package de.sciss.voicetrap
package impl

import de.sciss.lucre.{stm, DataOutput, DataInput}
import de.sciss.synth
import synth.proc
import proc.AuralSystem
import java.io.File

object DocumentImpl {
   private final val SER_VERSION = 1

   implicit object serializer extends stm.Serializer[ Tx, Acc, Document ] {
      def write( v: Document, out: DataOutput ) { v.write( out )}
      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Document = {
         readSerVersion( in, SER_VERSION )
         val id               = tx.readID( in, access )
         val cursor           = tx.readCursor( in, access )
         val groupVar         = tx.readVar[ ProcGroup ]( id, in ) // proc.ProcGroup_.Modifiable.read[ S ]( in, access )
         val channels         = mapSerializer[ (Int, Int), Channel ].read( in, access )
         val artifactStoreVar = tx.readVar[ ArtifactStore ]( id, in ) // .read[ S ]( in, access )
         new Impl( id, cursor, groupVar, channels, artifactStoreVar )
      }
   }

   def apply()( implicit tx: Tx ) : Document = {
      val id         = tx.newID()
      val cursor     = tx.newCursor()
      val groupVar   = tx.newVar( id, proc.ProcGroup_.Modifiable[ S ])
      val channels   = for( row <- 0 until VoiceTrap.numRows; column <- 0 until VoiceTrap.numColumns ) yield {
         (row, column) -> Channel( row, column )
      }
      val artifactStoreVar = tx.newVar( id, proc.ArtifactStore[ S ]( new File( VoiceTrap.baseDirectory, "artifacts" )))
      new Impl( id, cursor, groupVar, channels.toMap, artifactStoreVar )
   }

   private final class Impl( val id: ID, val cursor: Cursor, groupVar: Var[ ProcGroup ],
                             val channels: Map[ (Int, Int), Channel ],
                             val artifactStoreH : Var[ ArtifactStore ])
   extends Document {
      doc =>

      override def toString = "Document"

      def group( implicit tx: Tx ) : ProcGroup = groupVar.get
      def artifactStore( implicit tx: Tx ) : ArtifactStore = artifactStoreH.get

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
         id.write( out )
         cursor.write( out )
         groupVar.write( out )
         mapSerializer[ (Int, Int), Channel ].write( channels, out )
         artifactStoreH.write( out )
      }
   }
}