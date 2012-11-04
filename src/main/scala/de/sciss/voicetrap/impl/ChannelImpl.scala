/*
 *  ChannelImpl.scala
 *  (VoiceTrap)
 *
 *  Copyright (c) 2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.voicetrap
package impl

import de.sciss.lucre.{DataInput, DataOutput, stm}
import de.sciss.synth
import synth.{addAfter, SynthGraph, proc}
import concurrent.stm.Ref
import de.sciss.lucre.bitemp.Span
import java.io.File
import synth.proc.{RichServer, ProcTxn, RichSynthDef, Scan, Artifact, Grapheme}
import GraphemeUtil.formatSpan

import VoiceTrap.{numColumns, sampleRate, phraseLength, loopLength, forkIterations}

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

      override def toString = "chan_" + (row+1) + "_" + (column+1)

      def hiddenLayer : AudioArtifact = {
         val name    = "hidden_" + (row+1) + "_" + (column+1) + ".aif"
         val artif   = Artifact( name )
         val path    = new File( VoiceTrap.artifactDirectory, name ).getPath
         val spec    = audioFileSpec( path )
         Grapheme.Value.Audio( artif, spec, 0L, 1.0 )
      }

//      def cursor( implicit tx: Tx ) : Cursor = cursorVar.get

      def start( document: Document, server: RichServer, auralSystem: proc.AuralSystem[ S ])( implicit tx: Tx, cursor: Cursor ) {
         log( "spawning " + chan + " with " + cursor + " (pos = " + cursor.position + ")" )
         implicit val aStore  = document.artifactStore
         val transport        = proc.Transport[ S, I ]( group, VoiceTrap.sampleRate )
         val view = proc.AuralPresentation.run[ S, I ]( transport, auralSystem )
         view.group match {
            case Some( rg ) =>
               val routeGraph = SynthGraph {
                  import synth._
                  import ugen._
                  val inBus   = "in".kr( 0 )
                  val outBus  = "out".kr( 0 )
                  val sig     = In.ar( inBus, 1 )
                  Out.ar( outBus, sig ) // * SinOsc.ar( 444 )
                  ReplaceOut.ar( inBus, sig * DC.ar( 0 ))
               }
               implicit val ptx = ProcTxn()
               val sd = RichSynthDef( rg.server, routeGraph, nameHint = Some( "channel-route" ))
               val matrixIndex = row * numColumns + column
               sd.play( target = rg, args = Seq( "out" -> (VoiceTrap.privateBus.index + matrixIndex) ), addAction = addAfter )

            case _ => logThis( "! WARNING ! aural presentation does not exhibit a group" )
         }
         transport.play()
         transportVar.set( Some( transport ))( tx.peer )

//         testSpawn()
//         testReplay()

//         testRemove()
//         testAdd()

         nextSearch( 0, tx.info.timeStamp, document, server, transport )
      }

      def nextSearch( iter: Int, iterZeroTime: Long, document: Document, server: RichServer, transport: Transport )( implicit tx: Tx ) {
         implicit val itx = tx.peer
         val heuristic  = (sampleRate * 10.0).toLong  // XXX TODO
         val loop       = (loopLength.step() * sampleRate).toLong
         var timeNow    = transport.time
         if( timeNow >= loop ) {
            timeNow = 0L //    %= loop
            transport.seek( timeNow )
         }
         val insTime    = (timeNow + heuristic) % loop
//         search( insTime, (phraseLength.step() * sampleRate).toLong, group )
         val insSpan    = Span( insTime, insTime + (phraseLength.step() * sampleRate).toLong )

         implicit val aStore  = document.artifactStore
         val futArtifact = SearchStepAlgorithm( this, server, insSpan, group, hiddenLayer )
//         GraphemeUtil.threadTxn( "await search " + this ) { ... }
         awaitFuture( "await search " + this, futArtifact ) { futRes =>
//            val artOpt = futArtifact()
            val artOpt = futRes match {
               case FutureResult.Success( artifact ) =>
                  logThis( "search succeeded " + artifact )
                  Some( artifact )

               case FutureResult.Failure( e ) =>
                  logThis( "search failed" )
                  e.printStackTrace()
                  None
            }

            logThis( "running post search block" )
            document.cursor.step { implicit tx =>
               document.withChannel( row = row, column = column, jumpBack = None ) {
                  case (tx1, _, ch) =>
                     artOpt.foreach { artifact =>
                        val middle = insSpan.start + (insSpan.length / 2)
                        ch.removeAt( middle )
                        ch.insert( Grapheme.Segment.Audio( insSpan, artifact ))( tx1 )
//                              val transport = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
                     }
               }

               val nextIter   = (iter + 1) % forkIterations
               val iterTime   = tx.info.timeStamp
               val jumpBack   = if( nextIter == 0 ) Some( (iterTime + iterZeroTime) / 2 ) else None
               document.withChannel( row = row, column = column, jumpBack = jumpBack ) {
                  case (tx1, _, ch) =>
                     ch.nextSearch( nextIter, if( nextIter == 0 ) iterTime else iterZeroTime, document, server, transport )
               }
            }
         }
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

      def removeAt( time: Long )( implicit tx: Tx ) {
         val g = group
//         val transport = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
         group.intersect( time ).foreach { case (span, seq) =>
            seq.foreach { timed =>
               logThis( "removing process " + formatSpan( timed.span.value ) + " " + timed.value )
               /* val ok = */ g.remove( timed.span, timed.value )
   //de.sciss.lucre.event.showLog = false
//               logThis( "removing process - success? " + ok )
            }
         }
      }

      def insert( segm: AudioSegment )( implicit tx: Tx ) {
//         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))

         import synth._
         import ugen._
         import proc.graph.scan
         import implicits._

//         val spec    = AudioFile.readSpec( file )
//         val len     = math.min( spec.numFrames, (util.Random.nextDouble().linexp( 0.0, 1.0, 4.0, 20.0 ) * spec.sampleRate).toLong )
         val len     = segm.span match {
            case sp @ Span( _, _ ) => sp.length
            case _ => 44100L  // XXX
         }
         val dur     = len / sampleRate
//         val offset  = (util.Random.nextDouble() * (spec.numFrames - len)).toLong
//         val gain    = 0.5
//         val a       = Artifact( file.getName )
//         val aa      = Grapheme.Value.Audio( a, spec, offset, gain )
         val g       = group
         val time    = segm.span.start // transport.time
         val p       = proc.Proc[ S ]
         p.name_=( segm.value.artifact.toString )
         val scanw   = p.scans.add( "sig" )
         val scand   = p.scans.add( "dur" )
         val grw     = Grapheme.Modifiable[ S ]
         val grd     = Grapheme.Modifiable[ S ]
         grw.add( time -> segm.value )
         grd.add( time -> Grapheme.Value.Curve( dur -> stepShape ))
         scanw.source_=( Some( Scan.Link.Grapheme( grw )))
         scand.source_=( Some( Scan.Link.Grapheme( grd )))
         p.graph_=( SynthGraph {
            val sig     = scan( "sig" ).ar( 0 )
            val duri    = A2K.kr( scan( "dur" ).ar( 1 ))
            val env     = EnvGen.ar( Env.linen( 0.2, (duri - 0.4).max( 0 ), 0.2 ))
            Out.ar( 0, sig * env )
         })
         val span    = Span( time, time + len )
         logThis( "adding process " + formatSpan( span ) + " " + p + " in " + tx.inputAccess )
//de.sciss.lucre.event.showLog = true
         g.add( span, p )
//de.sciss.lucre.event.showLog = false
      }

//      private def testReplay()( implicit tx: Tx ) {
//         val g = group
//         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
//         transport.seek( 0L )
//         logThis( "FOUND in path " + tx.inputAccess + " : " + transport.iterator.toList )
////de.sciss.lucre.confluent.showLog = true
//         logThis( "GROUP " + g + " has first event at " + g.nearestEventAfter( Long.MinValue ))
//         transport.play()
//      }

//      private def testSpawn()( implicit tx: Tx ) {
//         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
//         val time       = transport.time
//         val g          = group
//
//         import implicits._
//
//         val p = proc.Proc[ S ]
//         p.name_=( chan.toString )
//         p.graph_=( synth.SynthGraph {
//            import synth._
//            import ugen._
//
//            val freq = (row * numColumns + column).linexp( 0, math.max( 1, matrixSize - 1 ), 300, 3000 )
//            val beat = LFPulse.ar( column.linexp( 0, math.max( 1, numColumns - 1 ), 1, 8.0/5 ))
//            val sig  = SinOsc.ar( freq ) * beat / matrixSize
//            Out.ar( 0, sig )
//         })
//
////         println( "ADDING in " + chan + " and path " + tx.inputAccess + " at " + time )
////
////de.sciss.lucre.confluent.showLog = true
//logThis( "ADDING TO GROUP " + g )
//         g.add( Span( time, time + (sampleRate * 4).toLong ), p )
////         de.sciss.lucre.confluent.showLog = true
//      }
   }
}