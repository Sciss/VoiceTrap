/*
 *  ChannelImpl.scala
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

import de.sciss.lucre.bitemp.{BiGroup, Span}
import de.sciss.lucre.{DataInput, DataOutput, data, stm}
import de.sciss.synth
import de.sciss.synth.proc.{Artifact, Grapheme, Proc, Scan, Server, Synth}
import de.sciss.synth.{SynthGraph, addAfter, proc}
import de.sciss.voicetrap.GraphemeUtil.formatSpan
import de.sciss.voicetrap.VoiceTrap.{forkIterations, loopLength, numColumns, phraseLength, sampleRate}

import java.io.File
import scala.concurrent.stm.{InTxn, Ref}

object ChannelImpl {
  private final val SER_VERSION = 1

  var VERBOSE = false

  implicit object serializer extends stm.Serializer[Tx, Acc, Channel] {
    def write(v: Channel, out: DataOutput): Unit = {
      v.write(out)
    }

    def read(in: DataInput, access: Acc)(implicit tx: Tx): Channel = {
      //         implicit val dtx: D#Tx = tx
      readSerVersion(in, "cha", SER_VERSION)
      val id = tx.readID(in, access)
      val row = in.readInt()
      val column = in.readInt()
      //         val cursorVar  = tx.readVar[ Cursor    ]( id, in )
      val groupVar = tx.readVar[ProcGroup](id, in)

      //         log( "read chan " + (row, column) + " -> cursor = " + cursorVar.get + " with path " + cursorVar.get.position )

      new Impl(id, row, column, /* cursorVar, */ groupVar)
    }
  }

  def apply(row: Int, column: Int, group: ProcGroup)(implicit tx: Tx): Channel = {
    //      val dtx: D#Tx  = tx
    val id = tx.newID()
    //      val initCursor = tx.newCursor()
    //      val cursorVar  = tx.newVar[ Cursor ]( id, initCursor )
    val groupVar = tx.newVar[ProcGroup](id, group)
    new Impl(id, row, column, /* cursorVar, */ groupVar)
  }

  private final class Impl(val id: ID, val row: Int, val column: Int, /* cursorVar: Var[ Cursor ], */ groupVar: Var[ProcGroup])
    extends Channel {
    chan =>

    private val transportVar = Ref(Option.empty[Transport])

    override def toString: String = "chan_" + (row + 1) + "_" + (column + 1)

    def hiddenLayer: AudioArtifact = {
      val name = "hidden_" + (row + 1) + "_" + (column + 1) + ".aif"
      val artif = Artifact(name)
      val path = new File(VoiceTrap.artifactDirectory, name).getPath
      val spec = audioFileSpec(path)
      Grapheme.Value.Audio(artif, spec, 0L, 1.0)
    }

    //      def cursor( implicit tx: Tx ) : Cursor = cursorVar.get

    def start(document: Document, server: Server, auralSystem: proc.AuralSystem[S])(implicit tx: Tx, cursor: Cursor): Unit = {
      log("spawning " + chan + " with " + cursor + " (pos = " + cursor.position + ")")
      implicit val aStore: ArtifactStore = document.artifactStore
      val loop = (loopLength.step()(tx.peer) * sampleRate).toLong
      nextSearch(loop, 0, tx.info.timeStamp, document, auralSystem, server, transportOption = None)
    }

    private def makeTransport(auralSystem: proc.AuralSystem[S])(implicit tx: Tx, cursor: Cursor,
                                                                artifactStore: ArtifactStore): Transport = {
      logThis("new transport")
      //            transportVar.get( tx.peer ).foreach { tOld =>
      //               tOld.stop()
      //            }
//      implicit val bridge: S#Tx => I#Tx = ???
      val t = proc.Transport[S, I](group, sampleRate)
      val view = proc.AuralPresentation.run[S, I](t, auralSystem)
      view.group match {
        case Some(rg) =>
//          implicit val ptx = ProcTxn()
          rg.moveBefore(audible = true, target = VoiceTrap.masterGroup)
          val routeGraph = SynthGraph {
            import synth._
            import ugen._
            val inBus = "in".kr(0)
            val outBus = "out".kr(0)
            val sig = In.ar(inBus, 1)
            Out.ar(outBus, sig) // * SinOsc.ar( 444 )
            ReplaceOut.ar(inBus, sig * DC.ar(0))
          }
//          val sd = RichSynthDef(rg.server, routeGraph, nameHint = Some("channel-route"))
          val matrixIndex = row * numColumns + column
          Synth(routeGraph, nameHint = Some("channel-route"))(target = rg,
            args = Seq("out" -> (VoiceTrap.privateBus.index + matrixIndex)), addAction = addAfter)

        // XXX TODO:
        //                  val pingGraph = SynthGraph {
        //
        //                  }

        case _ => logThis("! WARNING ! aural presentation does not exhibit a group")
      }
      t.play()
      transportVar.set(Some(t))(tx.peer)
      t
    }

    def nextSearch(loop: Long, iter: Int, iterZeroTime: Long, document: Document, auralSystem: proc.AuralSystem[S],
                   server: Server, transportOption: Option[Transport])(implicit tx: Tx, cursor: Cursor): Unit = {
      implicit val itx: InTxn = tx.peer

      implicit val aStore: ArtifactStore = document.artifactStore
      val transport = transportOption.getOrElse(makeTransport(auralSystem))

      val heuristic = (sampleRate * 10.0).toLong // XXX TODO
      var timeNow = transport.time
      if (timeNow >= loop) {
        timeNow = 0L //    %= loop
        transport.seek(timeNow)
        logThis("seek " + timeNow)
      }
      val insTime = (timeNow + heuristic) % loop
      //         search( insTime, (phraseLength.step() * sampleRate).toLong, group )
      val insSpan = Span(insTime, insTime + (phraseLength.step() * sampleRate).toLong)

      val futArtifact = SearchStepAlgorithm(this, server, insSpan, group, hiddenLayer)
      //         GraphemeUtil.threadTxn( "await search " + this ) { ... }
      awaitFuture("await search " + this, futArtifact) { futRes =>
        //            val artOpt = futArtifact()
        val artOpt = futRes match {
          case FutureResult.Success(artifact) =>
            logThis("search succeeded " + artifact)
            Some(artifact)

          case FutureResult.Failure(e) =>
            logThis("search failed")
            e.printStackTrace()
            None
        }

        logThis("running post search block")
        postStep(server, auralSystem, insSpan: Span, artOpt, document, transport, iter, iterZeroTime)
      }
    }

    private def playJumpBackSound(server: Server)(implicit ptx: Tx /*ProcTxn*/): Unit = {
      val gr = SynthGraph {
        import synth._
        import ugen._

        val bus = "bus".kr
        val amp = "amp".kr(1)
        val f = Line.ar(60, 120, dur = 2) // 80       // fundamental frequency
        val p = 10 // number of partials per channel
        val trig0 = XLine.kr(10, 0.1, 60) // trigger probability decreases over time
        val trig1 = EnvGen.kr(Env.linen(0, 2.75, 1, 1, stepShape))
        val trig = trig0 * trig1
        FreeSelf.kr(TDelay.kr(Done.kr(trig1), 1))
        val sig = Mix.tabulate(p) { i =>
          val dust = Dust.ar(trig)
          val freq = Latch.ar(in = f, trig = dust)
          val sig = SinOsc.ar(freq * (i + 1.5)) * // freq of partial
            Decay2.ar(
              dust * 0.02, // trigger amplitude
              0.005, // grain attack time
              Rand(0, 0.5) // grain decay time
            )
          sig
        }
        Out.ar(bus, sig * amp)
      }
//      val rd = RichSynthDef(server, gr)
      val ch = row * numColumns + column + VoiceTrap.privateBus.index
      Synth(gr)(target = server.defaultGroup, args = Seq("bus" -> ch, "amp" -> VoiceTrap.jumpBackSoundVolume))
    }

    private def postStep(server: Server, auralSystem: proc.AuralSystem[S], insSpan: Span,
                         artOpt: Option[AudioArtifact],
                         document: Document, transport: Transport, iter: Int, iterZeroTime: Long): Unit = {
      document.cursor.step { implicit tx =>
        val timeNow       = transport.time
        val loop          = (loopLength.step()(tx.peer) * sampleRate).toLong
        val incIter       = timeNow >= loop
        val nextIter      = if (incIter) (iter + 1) % forkIterations else iter
        val nextIterTime = tx.info.timeStamp
        val jumpBack      = if (incIter && (nextIter == 0)) Some((nextIterTime + iterZeroTime) / 2) else None

        if (incIter) logThis("iteration " + nextIter + jumpBack.map(" @" + _).getOrElse(""))

        if (jumpBack.isDefined && VoiceTrap.jumpBackSound) playJumpBackSound(server) // (ProcTxn())

        document.withChannel(row = row, column = column, jumpBack = None)(
          exchangeArtifact(artOpt, insSpan, jumpBack, transport))

        document.withChannel(row = row, column = column, jumpBack = jumpBack)(
          invokeNextSearch(loop, nextIter, if (jumpBack.isDefined) nextIterTime else iterZeroTime, document,
            auralSystem, server, if (jumpBack.isDefined) None else Some(transport))
        )
      }
    }

    private def exchangeArtifact(artOpt: Option[AudioArtifact], insSpan: Span, jumpBack: Option[Long],
                                 transport: Transport)(_tx: Tx, csr: Cursor, ch: Channel): Unit = {
      implicit val tx: Tx = _tx
      artOpt.foreach { artifact =>
        val middle = insSpan.start + (insSpan.length / 2)
        ch.removeAt(middle)
        ch.insert(Grapheme.Segment.Audio(insSpan, artifact))
        //                              val transport = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
      }

      if (jumpBack.isDefined) {
        ch.removeFrom(transport.time + (10 * sampleRate).toLong)
      }
    }

    private def invokeNextSearch(loop: Long, iter: Int, iterTimeZero: Long, document: Document,
                                 auralSystem: proc.AuralSystem[S], server: Server, transportOption: Option[Transport])
                                (_tx: Tx, _csr: Cursor, ch: Channel): Unit = {
      implicit val tx: Tx = _tx
      implicit val csr: Cursor = _csr
      ch.nextSearch(loop, iter, iterTimeZero, document, auralSystem, server, transportOption)
    }

    def stop()(implicit tx: Tx): Unit = {
      transportVar.get(tx.peer).foreach { t =>
        t.stop()
        t.dispose()
      }
    }

    //      def fork()( implicit tx: Tx ) {
    //         ...
    //      }

    def write(out: DataOutput): Unit = {
      writeSerVersion(out, "cha", SER_VERSION)
      id.write(out)
      out.writeInt(row)
      out.writeInt(column)
      //         cursorVar.write( out )
      groupVar.write(out)
    }

    def group(implicit tx: Tx): ProcGroup = groupVar.get

    //      def refresh( implicit tx: Tx ) : Channel = {
    //         tx.newHandle( this ).get
    //      }

    // ---- testing ----

    private def logThis(what: => String): Unit = {
      log(chan.toString + " : " + what)
    }

    def removeAt(time: Long)(implicit tx: Tx): Unit = {
      val g = group
      removeAll(g, g.intersect(time))
    }

    def removeFrom(time: Long)(implicit tx: Tx): Unit = {
      val g = group
      removeAll(g, g.intersect(Span.from(time)))
    }

    private def removeAll(g: ProcGroup, it: data.Iterator[S#Tx, BiGroup.Leaf[S, Proc[S]]])(implicit tx: Tx): Unit = {
      //         val g = group
      //         val transport = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))
      it.foreach { case (_, seq) =>
        seq.foreach { timed =>
          logThis("removing process " + formatSpan(timed.span.value) + " " + timed.value)
          /* val ok = */ g.remove(timed.span, timed.value)
          //de.sciss.lucre.event.showLog = false
          //               logThis( "removing process - success? " + ok )
        }
      }
    }

    def insert(segm: AudioSegment)(implicit tx: Tx): Unit = {
      //         val transport  = transportVar.get( tx.peer ).getOrElse( sys.error( "No transport" ))

      import implicits._
      import synth._
      import proc.graph.scan
      import ugen._

      //         val spec    = AudioFile.readSpec( file )
      //         val len     = math.min( spec.numFrames, (util.Random.nextDouble().linexp( 0.0, 1.0, 4.0, 20.0 ) * spec.sampleRate).toLong )
      val len = segm.span match {
        case sp@Span(_, _) => sp.length
        case _ => 44100L // XXX
      }
      val dur = len / sampleRate
      //         val offset  = (util.Random.nextDouble() * (spec.numFrames - len)).toLong
      //         val gain    = 0.5
      //         val a       = Artifact( file.getName )
      //         val aa      = Grapheme.Value.Audio( a, spec, offset, gain )
      val g = group
      val time = segm.span.start // transport.time
      val p = proc.Proc[S]
      p.name_=(segm.value.artifact.toString)
      val scanw = p.scans.add("sig")
      val scand = p.scans.add("dur")
      val grw = Grapheme.Modifiable[S]
      val grd = Grapheme.Modifiable[S]
      grw.add(time -> segm.value)
      grd.add(time -> Grapheme.Value.Curve(dur -> stepShape))
      scanw.source_=(Some(Scan.Link.Grapheme(grw)))
      scand.source_=(Some(Scan.Link.Grapheme(grd)))
      p.graph_=(SynthGraph {
        val sig = scan("sig").ar(0)
        val duri = A2K.kr(scan("dur").ar(1))
        val env = EnvGen.ar(Env.linen(0.2, (duri - 0.4).max(0), 0.2))
        Out.ar(0, sig * env)
      })
      val span = Span(time, time + len)
      logThis("adding process " + formatSpan(span) + " " + p + " in " + tx.inputAccess)
      //de.sciss.lucre.event.showLog = true
      g.add(span, p)
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