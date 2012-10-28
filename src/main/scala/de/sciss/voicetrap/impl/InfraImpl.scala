package de.sciss.voicetrap
package impl

import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.synth.{proc, Server}
import proc.AuralSystem
import de.sciss.osc

object InfraImpl {
   def apply() : Infra = {
      val dir     = new File( VoiceTrap.baseDirectory, "db" )
      val store   = BerkeleyDB.factory( dir )
      val system  = ConfluentReactive( store )
//de.sciss.lucre.confluent.showLog = true
      val (access, cursor) = system.cursorRoot[ Document, Cursor ] { implicit tx => Document() } { _ => _.cursor }
//de.sciss.lucre.confluent.showLog = false
      new Impl( system, access )( cursor )
   }

   private final class Impl( val system: S, val document: Source[ Document ])( implicit cursor: Cursor )
   extends Infra {
      def start() {
         val sCfg = Server.Config()
         sCfg.deviceName         = Some( VoiceTrap.audioInterface )
         sCfg.inputBusChannels   = VoiceTrap.highestInputChannel
         sCfg.outputBusChannels  = VoiceTrap.highestOutputChannel
         sCfg.loadSynthDefs      = false
         sCfg.transport          = osc.TCP
         sCfg.pickPort()

         cursor.step { implicit tx =>
            log( "infra started with path " + tx.inputAccess )
            val as = AuralSystem[ S ].start( sCfg )
            as.whenStarted { implicit tx =>
               server =>
                  server.peer.dumpOSC()
                  proc.showLog      = true
                  proc.showAuralLog = true
                  document.get.start( as )
            }
         }
      }
   }
}