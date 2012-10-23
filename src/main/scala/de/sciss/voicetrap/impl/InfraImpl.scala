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
      val (access, cursor) = system.cursorRoot[ Document, Cursor ] { implicit tx => Document() } { _ => _.cursor }

      new Impl( system, access, cursor )
   }

   private final class Impl( val system: S, val document: Source[ Document ], cursor: Cursor )
   extends Infra {
      def start() {
         cursor.step { implicit tx =>
            document.get.start()
         }
      }
   }
}