/*
 *  LiveTelevisionImpl.scala
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

import java.io.File
import de.sciss.synth
import synth.io.{AudioFile, SampleFormat, AudioFileType}
import concurrent.stm.{InTxn, Ref}

object LiveTelevisionImpl {
   private val identifier  = "live-television-impl"

   def apply() : Television = new LiveTelevisionImpl()
}
final class LiveTelevisionImpl private () extends Television {
   import GraphemeUtil._
   import LiveTelevisionImpl._

   val lookAheadLim = 0.01

   def latency = lookAheadLim * 2

//   private val procRef  = Ref( Option.empty[ synth.proc.Proc ])
   private val futRef   = Ref({
      val ev = FutureResult.event[ File ]()
      ev.fail( new RuntimeException( identifier + " : capture file not yet initialized" ))
      ev
   })

   def capture( length: Long )( implicit tx: InTxn ) : FutureResult[ File ] = {
      import synth._
      import ugen._
      import proc._
//      import DSL._

      val dur     = framesToSeconds( length ) + latency
      val res     = FutureResult.event[ File ]()
      val oldFut  = futRef.swap( res )
      require( oldFut.isSet, identifier + " : still in previous capture" )

      val graph   = SynthGraph {
         val in      = In.ar( NumOutputBuses.ir + VoiceTrap.microphoneChannel, 1 )
         val boost   = "boost".kr
         val mix     = Limiter.ar( Mix.mono( in ) * boost, 0.97, 0.01 )
         val buf     = "buf".ir
         val dura    = "dur".ir
//         val done    = Done.kr( Line.kr( dur = dura ))
         Line.kr( dur = dura, doneAction = freeSelf )
         DiskOut.ar( buf, mix )
      }

      val server: RichServer = ???

      val rd   = RichSynthDef( server, graph )
      val path = createTempFile( ".aif", None, keep = false )
//      val buf = bufRecord( path.getAbsolutePath, 1, AudioFileType.AIFF, SampleFormat.Int24 )
      val buf  = RichBuffer( server )
      buf.alloc( numFrames = 32768, numChannels = 1 )
      buf.record( path.getAbsolutePath, AudioFileType.AIFF, SampleFormat.Int24 )
      val rs = rd.play(
         target = server.defaultGroup,
         args = Seq( "boost" -> VoiceTrap.microphoneGain, "dur" -> dur, "buf" -> buf.id ),
         buffers = Seq( buf )
      )

      rs.onEndTxn { implicit ptx =>
         buf.closeAndFree()
         threadTxn( identifier + " : capture completed" ) {
            ???
         }
      }

//      p.control( "dur" ).v = dur
//      p.play
// XXX TODO : this should be somewhat handled (ProcTxn needs addition)
//      tx.afterFailure { e => res.fail( e )}

//      res
      ???
   }
}