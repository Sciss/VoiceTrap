package de.sciss.voicetrap
package impl

import de.sciss.lucre.{DataInput, DataOutput, stm}
import de.sciss.synth
import synth.proc
import concurrent.stm.Ref
import de.sciss.lucre.bitemp.Span

object ChannelImpl {
   private final val SER_VERSION = 1

   var VERBOSE = false

   implicit object cursorSerializer extends stm.Serializer[ Tx, Acc, Cursor ] {
      def write( v: Cursor, out: DataOutput ) { v.write( out )}
      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Cursor = {
         tx.readCursor( in, access )
      }
   }

   implicit object serializer extends stm.Serializer[ Tx, Acc, Channel ] {
      def write( v: Channel, out: DataOutput ) { v.write( out )}

      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Channel = {
//         implicit val dtx: D#Tx = tx
         readSerVersion( in, "cha", SER_VERSION )
         val id         = tx.readID( in, access )
         val row        = in.readInt()
         val column     = in.readInt()
         val cursorVar  = tx.readVar[ Cursor ]( id, in )
         new Impl( id, row, column, cursorVar )
      }
   }

   def apply( row: Int, column: Int )( implicit tx: Tx ) : Channel = {
//      val dtx: D#Tx  = tx
      val id         = tx.newID()
      val initCursor = tx.newCursor()
      val cursorVar  = tx.newVar[ Cursor ]( id, initCursor )
      new Impl( id, row, column, cursorVar )
   }

   private final class Impl( val id: ID, val row: Int, val column: Int, cursorVar: Var[ Cursor ])
   extends Channel {
      chan =>

      private val transportVar = Ref( Option.empty[ Transport ])

      override def toString = "Chan(r=" + row + ", c=" + column + ")"

      def hiddenLayer : AudioArtifact = ???

//      def cursor( implicit tx: Tx ) : Cursor = cursorVar.get

      def start( document: Document, auralSystem: proc.AuralSystem[ S ])( implicit tx: Tx ) {
//         implicit val dtx: D#Tx  = tx

//         implicit val cursor = cursorVar.get
         val newCursor = tx.newCursor()
         val oldCursor = cursorVar.get
         cursorVar.set( newCursor )
         oldCursor.dispose()

         log( "spawning " + chan + " with path " + newCursor.position )

         spawn( newCursor ) { implicit tx =>
            log( "spawned " + chan )
            start2( document, auralSystem )( tx, newCursor )
         }
      }

      private def start2( document: Document, auralSystem: proc.AuralSystem[ S ])( implicit tx: Tx, cursor: Cursor ) {
         implicit val aStore  = document.artifactStore
         val transport        = proc.Transport[ S, I ]( document.group, VoiceTrap.sampleRate )
         /* val view = */ proc.AuralPresentation.run[ S, I ]( transport, auralSystem )
         transport.play()
         transportVar.set( Some( transport ))( tx.peer )

//         testSpawn( document.group )
         testReplay( document.group )
      }

      def stop()( implicit tx: Tx ) {
         transportVar.get( tx.peer ).foreach { t =>
            t.stop()
            t.dispose()
         }
      }

//      def fork()( implicit tx: Tx ) {
//         ...
//      }

      def write( out: DataOutput ) {
         writeSerVersion( out, "cha", SER_VERSION )
         id.write( out )
         out.writeInt( row )
         out.writeInt( column )
         cursorVar.write( out )
      }

      // ---- testing ----

      private def testReplay( group: ProcGroup )( implicit tx: Tx ) {
         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
         transport.seek( 0L )
         println( "FOUND in " + chan + " and path " + tx.inputAccess + " : " + transport.iterator.toList )
de.sciss.lucre.confluent.showLog = true
         println( "group has first event at " + group.nearestEventAfter( Long.MinValue ))
         transport.play()
      }

      private def testSpawn( group: ProcGroup )( implicit tx: Tx ) {
         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
         val time       = transport.time

         import implicits._
         import VoiceTrap.{numColumns, numRows, matrixSize, sampleRate}

         val p = proc.Proc[ S ]
         p.name_=( chan.toString )
         p.graph_=( synth.SynthGraph {
            import synth._
            import ugen._

            val freq = (row * numColumns + column).linexp( 0, math.max( 1, matrixSize - 1 ), 300, 3000 )
            val beat = LFPulse.ar( column.linexp( 0, math.max( 1, numColumns - 1 ), 1, 8.0/5 ))
            val sig  = SinOsc.ar( freq ) * beat / matrixSize
            Out.ar( 0, sig )
         })

         println( "ADDING in " + chan + " and path " + tx.inputAccess + " at " + time )

de.sciss.lucre.confluent.showLog = true
         group.add( Span( time, time + (sampleRate * 4).toLong ), p )
//         de.sciss.lucre.confluent.showLog = true
      }
   }
}