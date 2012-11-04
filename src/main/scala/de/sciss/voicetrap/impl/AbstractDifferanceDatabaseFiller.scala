/*
 *  AbstractDifferanceDatabaseFiller.scala
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
import concurrent.stm.InTxn
import de.sciss.synth.proc.RichServer

abstract class AbstractDifferanceDatabaseFiller extends DifferanceDatabaseFiller {
   import GraphemeUtil._

//   private val identifier = "a-database-filler"

   /**
    * Target database length in seconds.
    */
   def durationMotion : Motion

   def database : Database
   def television : Television
   def maxCaptureDur : Double

   def perform( server: RichServer )( implicit tx: Tx ) : FutureResult[ Unit ] = {
      implicit val itx = tx.peer
      val tgtLen  = secondsToFrames( durationMotion.step() )
      val dbLen   = database.length
      val inc0    = tgtLen - dbLen
      val maxF    = secondsToFrames( maxCaptureDur )
      val inc     = min( inc0, maxF )

      if( inc > 44100L /* 0 */ ) {
         val fillMsg = identifier + " fill"
         log( identifier + " : gathering " + formatSeconds( framesToSeconds( inc )))
         television.capture( identifier, server, inc ).flatMapSuccess( fillMsg ) { f =>
//            log( identifier + " : mapping capture success" )
            performWithFile( f, secondsToFrames( television.latency ), inc )
         }
      } else {
         futureOf( identifier + " no fill needed", () )
      }
   }

   private def performWithFile( file: File, off: Long, inc: Long ) : FutureResult[ Unit ] = {
      atom( identifier + " : appending " + formatSeconds( framesToSeconds( inc ))) { tx1 =>
         database.append( file, off, inc )( tx1 )
      }
   }
}