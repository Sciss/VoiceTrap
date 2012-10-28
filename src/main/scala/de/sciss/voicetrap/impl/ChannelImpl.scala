package de.sciss.voicetrap
package impl

import de.sciss.lucre.{DataInput, DataOutput, stm}
import de.sciss.synth
import synth.io.AudioFile
import synth.proc
import concurrent.stm.Ref
import de.sciss.lucre.bitemp.Span
import java.io.{IOException, File, FilenameFilter}
import synth.proc.{Scan, Artifact, Grapheme}

object ChannelImpl {
   private final val SER_VERSION = 1

   var VERBOSE = false

   implicit object serializer extends stm.Serializer[ Tx, Acc, Channel ] {
      def write( v: Channel, out: DataOutput ) { v.write( out )}

      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Channel = {
//         implicit val dtx: D#Tx = tx
         readSerVersion( in, "cha", SER_VERSION )
         val id         = tx.readID( in, access )
         val row        = in.readInt()
         val column     = in.readInt()
//         val cursorVar  = tx.readVar[ Cursor    ]( id, in )
         val groupVar   = tx.readVar[ ProcGroup ]( id, in )

//         log( "read chan " + (row, column) + " -> cursor = " + cursorVar.get + " with path " + cursorVar.get.position )

         new Impl( id, row, column, /* cursorVar, */ groupVar )
      }
   }

   def apply( row: Int, column: Int, group: ProcGroup )( implicit tx: Tx ) : Channel = {
//      val dtx: D#Tx  = tx
      val id         = tx.newID()
//      val initCursor = tx.newCursor()
//      val cursorVar  = tx.newVar[ Cursor ]( id, initCursor )
      val groupVar   = tx.newVar[ ProcGroup ]( id, group )
      new Impl( id, row, column, /* cursorVar, */ groupVar )
   }

   private final class Impl( val id: ID, val row: Int, val column: Int, /* cursorVar: Var[ Cursor ], */ groupVar: Var[ ProcGroup ])
   extends Channel {
      chan =>

      private val transportVar = Ref( Option.empty[ Transport ])

      override def toString = "Chan" + id + "(r=" + row + ", c=" + column + ")"

      def hiddenLayer : AudioArtifact = ???

//      def cursor( implicit tx: Tx ) : Cursor = cursorVar.get

      def start( document: Document, auralSystem: proc.AuralSystem[ S ])( implicit tx: Tx, cursor: Cursor ) {
         log( "spawning " + chan + " with path " + cursor.position )
         implicit val aStore  = document.artifactStore
         val transport        = proc.Transport[ S, I ]( group, VoiceTrap.sampleRate )
         /* val view = */ proc.AuralPresentation.run[ S, I ]( transport, auralSystem )
         transport.play()
         transportVar.set( Some( transport ))( tx.peer )

//         testSpawn()
//         testReplay()

         testRemove()
         testAdd()
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
//         cursorVar.write( out )
         groupVar.write( out )
      }

      def group( implicit tx: Tx ) : ProcGroup = groupVar.get

//      def refresh( implicit tx: Tx ) : Channel = {
//         tx.newHandle( this ).get
//      }

      // ---- testing ----

      private def logThis( what: => String ) {
         log( chan.toString + " : " + what )
      }

      private def testRemove()( implicit tx: Tx ) {
         val g = group
         val transport = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
         transport.iterator.toList.headOption.foreach { case (span, timed) =>
            logThis( "removing process " + timed )
//de.sciss.lucre.event.showLog = true
            val ok = g.remove( timed.span, timed.value )
//de.sciss.lucre.event.showLog = false
            logThis( "removing process - success? " + ok )
         }
      }

      private def testAdd()( implicit tx: Tx ) {
         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
         val files = Option( VoiceTrap.artifactDirectory.listFiles( new FilenameFilter {
            def accept( dir: File, name: String ) = name.endsWith( ".aif" )
         })).getOrElse( Array.empty ).toIndexedSeq
         if( files.nonEmpty ) {
            val i       = util.Random.nextInt( files.size )
            val file    = files( i )
            try {
               import implicits._
               import synth._
               import ugen._
               import proc.graph.scan
               val spec    = AudioFile.readSpec( file )
               val len     = math.min( spec.numFrames, (util.Random.nextDouble().linexp( 0.0, 1.0, 4.0, 20.0 ) * spec.sampleRate).toLong )
               val dur     = len / spec.sampleRate
               val offset  = (util.Random.nextDouble() * (spec.numFrames - len)).toLong
               val gain    = 0.5
               val a       = Artifact( file.getName )
               val aa      = Grapheme.Value.Audio( a, spec, offset, gain )
               val g       = group
               val time    = transport.time
               val p       = proc.Proc[ S ]
               p.name_=( "file:" + file.getName )
               val scanw   = p.scans.add( "sig" )
               val scand   = p.scans.add( "dur" )
               val grw     = Grapheme.Modifiable[ S ]
               val grd     = Grapheme.Modifiable[ S ]
               grw.add( time -> aa )
               grd.add( time -> Grapheme.Value.Curve( dur -> stepShape ))
               scanw.source_=( Some( Scan.Link.Grapheme( grw )))
               scand.source_=( Some( Scan.Link.Grapheme( grd )))
               p.graph_=( SynthGraph {
                  val sig  = scan( "sig" ).ar( 0 )
                  val duri = A2K.kr( scan( "dur" ).ar( 1 ))
                  val env  = EnvGen.ar( Env.linen( 0.2, (duri - 0.4).max( 0 ), 0.2 ))
                  Out.ar( 0, sig * env )
               })
               val span    = Span( time, time + len )
               logThis( "adding process " + (span, p) )
//de.sciss.lucre.event.showLog = true
               g.add( span, p )
//de.sciss.lucre.event.showLog = false

            } catch {
               case e: IOException => e.printStackTrace()
            }
         }
      }

      private def testReplay()( implicit tx: Tx ) {
         val g = group
         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
         transport.seek( 0L )
         logThis( "FOUND in path " + tx.inputAccess + " : " + transport.iterator.toList )
//de.sciss.lucre.confluent.showLog = true
         logThis( "GROUP " + g + " has first event at " + g.nearestEventAfter( Long.MinValue ))
         transport.play()
      }

      private def testSpawn()( implicit tx: Tx ) {
         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
         val time       = transport.time
         val g          = group

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

//         println( "ADDING in " + chan + " and path " + tx.inputAccess + " at " + time )
//
//de.sciss.lucre.confluent.showLog = true
logThis( "ADDING TO GROUP " + g )
         g.add( Span( time, time + (sampleRate * 4).toLong ), p )
//         de.sciss.lucre.confluent.showLog = true
      }
   }
}