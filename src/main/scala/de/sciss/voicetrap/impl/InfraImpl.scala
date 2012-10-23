package de.sciss.voicetrap
package impl

import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File

object InfraImpl {
   def apply() : Infra = {
      val dir     = new File( VoiceTrap.baseDirectory, "db" )
      val store   = BerkeleyDB.factory( dir )
      val system  = ConfluentReactive( store )
      val access  = system.root[ Document ] { implicit tx => Document() }

      new Impl( system, access )
   }

   private final class Impl( val system: S, val document: Source[ Document ]) extends Infra {
      def start() {
         ??? // document.get.start()
      }
   }
}