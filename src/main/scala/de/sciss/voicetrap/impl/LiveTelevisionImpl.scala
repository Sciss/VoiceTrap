/*
 *  LiveTelevisionImpl.scala
 *  (VoiceTrap)
 *
 *  Copyright (c) 2012-2021 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.synth
import de.sciss.synth.io.{AudioFile, AudioFileType, SampleFormat}
import de.sciss.synth.proc.Server

import java.io.{File, IOException}
import scala.concurrent.stm.InTxn

object LiveTelevisionImpl {
  //   private val identifier  = "live-television-impl"

  def apply(): Television = new LiveTelevisionImpl()
}

final class LiveTelevisionImpl private() extends Television {

  import GraphemeUtil._

  val lookAheadLim = 0.01
  //   val DEBUG = false

  def latency: Double = lookAheadLim * 2

  //   private val procRef  = Ref( Option.empty[ synth.proc.Proc ])
  //   private val futRef   = Ref({
  //      val ev = FutureResult.event[ File ]()
  //      ev.fail( new RuntimeException( identifier + " : capture file not yet initialized" ))
  //      ev
  //   })

  def capture(identifier: String, server: Server, length: Long)(implicit tx: Tx): FutureResult[File] = {
    import synth.{Buffer => _, Synth => _, _}
    import proc.{log => _, _}
    import ugen._
    //      import DSL._

    val dur = framesToSeconds(length) + latency
    val res = FutureResult.event[File](identifier + " capture")
    //      val oldFut  = futRef.swap( res )( tx.peer )
    //      require( oldFut.isSet, identifier + " : still in previous capture" )

    //      if( DEBUG ) log( identifier + " : capture begin [1]" )

    val graph = SynthGraph {
      val boost = "boost".kr
      val buf = "buf".kr // ir
      val dura = "dur".ir
      val in0 = In.ar(NumOutputBuses.ir + VoiceTrap.microphoneChannel, 1)
      val in1 = if (VoiceTrap.hpfFreq >= 16) HPF.ar(in0, VoiceTrap.hpfFreq) else in0
      val in = if (VoiceTrap.compander) {
        Compander.ar(in1, in1, thresh = (-24).dbamp, ratioBelow = 1, ratioAbove = 0.33, attack = 0.2, release = 1)
      } else in1

      val mix = Limiter.ar(in * boost, 0.97, 0.01)
      //         val done    = Done.kr( Line.kr( dur = dura ))
      Line.kr(dur = dura, doneAction = freeSelf)
      DiskOut.ar(buf, mix) // WhiteNoise.ar( 0.2 )) // mix * DC.ar(0) )
    }

    //      if( DEBUG ) log( identifier + " : capture begin [2]" )

//    implicit val ptx = ProcTxn()
//    val rd = RichSynthDef(server, graph)
    val path = createTempFile(".aif", None, keep = false)
    //      val buf = bufRecord( path.getAbsolutePath, 1, AudioFileType.AIFF, SampleFormat.Int24 )
    val buf = Buffer.diskOut(server)(path.getAbsolutePath, AudioFileType.AIFF, SampleFormat.Int24,
      numFrames = VoiceTrap.recordBufferSize, numChannels = 1)

    log(identifier + " : capture begin " + path)

    //      if( DEBUG ) log( identifier + " : capture begin [3]" )

    val rs = Synth(graph)(
      target = server.defaultGroup,
      args = Seq("boost" -> VoiceTrap.microphoneGain, "dur" -> dur, "buf" -> buf.id),
      dependencies /*buffers*/ = List(buf)
    )

    //      if( DEBUG ) log( identifier + " : capture begin [4]" )

    //val thr = Thread.currentThread()

    rs.onEndTxn { implicit ptx =>
      log(identifier + " : capture closing ") // + (Thread.currentThread() == thr) )
      buf.dispose() // closeAndFree()
      // trick to make the transaction commit wait for the buffer closing confirmation

// DISABLED 2021
//      ptx.addMessage(message = osc.StatusMessage, /*change = None,*/ audible = true, dependencies = List(buf))

      implicit val itx: InTxn = ptx.peer
      submitTxn { // threadTxn( identifier + " : capture completed" )
        finishCapture(identifier, path, res)
      } { e =>
        res.fail(e)
      }
    }

    //      if( DEBUG ) log( identifier + " : capture begin [5]" )

    // XXX TODO : this should be somewhat handled (ProcTxn needs addition)
    //      tx.afterFailure { e => res.fail( e )}

    res
  }

  private def finishCapture(identifier: String, path: File, fut: FutureResult.Event[File]): Unit = {
    //      requireTxnThread()
    try {
      AudioFile.readSpec(path)
      log(identifier + " : capture completed")
      fut.succeed(path)
    } catch {
      case e: IOException =>
        log(identifier + " : capture failed")
        fut.fail(e)
    }
  }
}
