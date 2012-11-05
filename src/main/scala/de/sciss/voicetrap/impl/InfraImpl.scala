package de.sciss.voicetrap
package impl

import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.synth
import synth.{addToTail, SynthGraph, Bus, proc, Server}
import synth.proc.{RichGroup, SoundProcesses, ProcTxn, RichSynthDef, AuralSystem}
import de.sciss.osc

object InfraImpl {
   def apply() : Infra = {
      // prevent actor starvation!!!
      // --> http://scala-programming-language.1934581.n4.nabble.com/Scala-Actors-Starvation-td2281657.html
      // DOESN'T HELP
      System.setProperty( "actors.enableForkJoin", "false" )

//      // DOESN'T HELP
//      SoundProcesses.poolSize = Some( 2 )
      SoundProcesses.cueBufferSize = 65536 // 32678

      val dir     = new File( VoiceTrap.baseDirectory, "db" )
      val store   = BerkeleyDB.factory( dir )
      val system  = ConfluentReactive( store )
//de.sciss.lucre.confluent.showCursorLog = true
//de.sciss.lucre.confluent.showLog       = true
//de.sciss.lucre.event.showLog           = true
      val (access, cursor) = system.cursorRoot[ Document, Cursor ] { implicit tx => Document() } { _ => _.cursor }
//de.sciss.lucre.confluent.showLog = false
      log( "main cursor is " + cursor )
      new Impl( system, access )( cursor )
   }

   private final class Impl( val system: S, val document: Source[ Document ])( implicit cursor: Cursor )
   extends Infra {
      def start() {
         log( "infra starting" )
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

      private def boot() {
         val sCfg = Server.Config()
         sCfg.deviceName         = Some( VoiceTrap.audioInterface )
         sCfg.inputBusChannels   = VoiceTrap.highestInputChannel
         sCfg.outputBusChannels  = VoiceTrap.highestOutputChannel
         sCfg.loadSynthDefs      = false
         sCfg.transport          = VoiceTrap.protocol
         if( VoiceTrap.bootServer ) {
            sCfg.pickPort()
         } else {
            sCfg.port   = 57110
         }

         cursor.step { implicit tx =>
            log( "infra booted with path " + tx.inputAccess )
            val as = AuralSystem[ S ].start( sCfg, connect = !VoiceTrap.bootServer )
            as.whenStarted { implicit tx =>
               server =>
                  val internalBus = Bus.audio( server.peer, numChannels = VoiceTrap.matrixSize )
                  VoiceTrap.privateBus = internalBus
                  val routeGraph = SynthGraph {
                     import synth._
                     import ugen._
                     val sig0 = In.ar( internalBus.index, numChannels = internalBus.numChannels )
                     if( VoiceTrap.stereoOutput ) {
                        Out.ar( 0, SplayAz.ar( 2, sig0 ))
                     } else {
                        assert( VoiceTrap.outChannels.size == internalBus.numChannels )
                        Vector.tabulate( internalBus.numChannels ) { ch =>
                           val chSig = sig0.\( ch )
                           val outCh = VoiceTrap.outChannels( ch )
                           Out.ar( outCh, Limiter.ar( chSig, level = VoiceTrap.limiterLevel ))
                        }
                     }
                  }
                  implicit val ptx = ProcTxn()
                  val rd = RichSynthDef( server, routeGraph, nameHint = Some( "internal-bus" ))
                  val mg = RichGroup( server )
                  mg.play( target = server.defaultGroup, addAction = addToTail )
                  rd.play( target = mg )
                  VoiceTrap.masterGroup = mg

                  server.peer.dumpOSC()
//                  proc.showAllocLog = true
//                  proc.showTxnLog   = true

//                  proc.showLog      = true
//                  proc.showAuralLog = true

//                  proc.showTransportLog = true
                  document.get.start( server, as )

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
