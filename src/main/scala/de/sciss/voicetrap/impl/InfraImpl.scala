/*
 *  InfraImpl.scala
 *  (VoiceTrap)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.stm
import stm.store.BerkeleyDB
import java.io.File
import de.sciss.synth
import synth.{addToTail, SynthGraph, Bus, proc, Server}
import proc.{SoundProcesses, AuralSystem}

object InfraImpl {
  def apply(): Infra = {
    // prevent actor starvation!!!
    // --> http://scala-programming-language.1934581.n4.nabble.com/Scala-Actors-Starvation-td2281657.html
    // DOESN'T HELP
    System.setProperty("actors.enableForkJoin", "false")

    //      // DOESN'T HELP
    //      SoundProcesses.poolSize = Some( 2 )
    SoundProcesses.cueBufferSize = 65536 // 32678

    val dir = new File(VoiceTrap.baseDirectory, "db")
    val store = BerkeleyDB.factory(dir)
    val system = proc.Confluent(store)
    //de.sciss.lucre.confluent.showCursorLog = true
    //de.sciss.lucre.confluent.showLog       = true
    //de.sciss.lucre.event.showLog           = true
    val (access, cursor) = system.cursorRoot[Document, Cursor] {
      implicit tx => Document()
    } {
      _ => _.cursor
    }
    //de.sciss.lucre.confluent.showLog = false
    log("main cursor is " + cursor)
    new Impl(system, access)(cursor)
  }

  private final class Impl(val system: S, val document: Source[Document])(implicit val cursor: Cursor)
    extends Infra {

    def start(): Unit = {
      log("infra starting")
      //         val futFill = atom( "database fill" ) { implicit tx => VoiceTrap.databaseFiller.perform() }
      //         GraphemeUtil.thread( "infra boot" ) {
      //            futFill() match {
      //               case FutureResult.Success( _ ) =>
      //                  log( "database filled" )
      submit {
        VoiceTrap.txnThread = Thread.currentThread()
        boot()
      }
      //               case FutureResult.Failure( e ) =>
      //                  log( "database fill failed" )
      //                  e.printStackTrace()
      //                  // XXX TODO: reboot application
      //            }
      //         }
    }

    private def boot(): Unit = {
      val sCfg = Server.Config()
      sCfg.deviceName         = Some(VoiceTrap.audioInterface)
      sCfg.inputBusChannels   = VoiceTrap.highestInputChannel
      sCfg.outputBusChannels  = VoiceTrap.highestOutputChannel
      sCfg.loadSynthDefs      = false
      sCfg.transport          = VoiceTrap.protocol
      if (VoiceTrap.bootServer) {
        sCfg.pickPort()
      } else {
        sCfg.port = 57110
      }

      cursor.step { implicit tx =>
        log("infra booted with path " + tx.inputAccess)
        val as = AuralSystem[S].start(sCfg, connect = !VoiceTrap.bootServer)
        as.whenStarted { implicit tx => server =>
          val internalBus = Bus.audio(server.peer, numChannels = VoiceTrap.matrixSize)
          VoiceTrap.privateBus = internalBus
          val routeGraph = SynthGraph {
            import synth._
            import ugen._
            val sig0 = In.ar(internalBus.index, numChannels = internalBus.numChannels) * VoiceTrap.masterGain
            if (VoiceTrap.stereoOutput) {
              Out.ar(0, SplayAz.ar(2, sig0))
            } else {
              assert(VoiceTrap.outChannels.size == internalBus.numChannels)
              Vector.tabulate(internalBus.numChannels) { ch =>
                val chSig = sig0.\(ch)
                val outCh = VoiceTrap.outChannels(ch)
                Out.ar(outCh, Limiter.ar(chSig, level = VoiceTrap.limiterLevel))
              }
            }
          }
          //                  implicit val ptx = ProcTxn()
          //                  val rd = proc.SynthDef( server, routeGraph, nameHint = Some( "internal-bus" ))
          val mg = proc.Group(server)(target = server.defaultGroup, addAction = addToTail)
          //                  mg.play( target = server.defaultGroup, addAction = addToTail )
          //                  rd.play( target = mg )
          proc.Synth(routeGraph, nameHint = Some("internal-bus"))(target = mg)

          VoiceTrap.masterGroup = mg

          if (VoiceTrap.dumpOSC) server.peer.dumpOSC()
          de.sciss.voicetrap.showLog = true
          //                  proc.showAllocLog = true
          //                  proc.showTxnLog   = true

          //                  proc.showLog      = true
          //                  proc.showAuralLog = true

          //                  proc.showTransportLog = true
          document.get.start(server, as)

          //new Thread( "dump futs" ) {
          //   start()
          //   override def run() {
          //      Thread.sleep( 90 * 1000L )
          //      FutureResult.dumpOpenFutures()
          //   }
          //}

        }
      }
    }
  }
}