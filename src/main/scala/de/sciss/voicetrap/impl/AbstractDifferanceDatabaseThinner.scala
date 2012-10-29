/*
 *  AbstractDifferanceDatabaseThinner.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.InTxn
import de.sciss.lucre.bitemp.Span

//object AbstractDifferanceDatabaseThinner {
//   private final case class Entry( span: Span, fadeIn: Long, fadeOut: Long ) {
//      def merge( b: Entry ) : Option[ Entry ] = {
//         if( span.overlaps( b.span )) {
//            val spanNew = span.unite( b.span )
//            Some( Entry( spanNew, if( spanNew.start == span.start ) fadeIn else b.fadeIn,
//                                  if( spanNew.stop == span.stop ) fadeOut else b.fadeOut ))
//         } else None
//      }
//   }
//}
abstract class AbstractDifferanceDatabaseThinner extends DifferanceDatabaseThinner {
   import GraphemeUtil._
//   import AbstractDifferanceDatabaseThinner._

   /**
    * Cross-fade punch-in duration in seconds
    */
   def fadeInMotion : Motion
   /**
    * Cross-fade punch-out duration in seconds
    */
   def fadeOutMotion : Motion
   /**
    * Factor at which the removed spans shrink (0 = no shrinking, 1 = removal not carried out)
    */
   def shrinkMotion : Motion
   /**
    * Position jitter in seconds
    */
   def jitterMotion : Motion

   def database : Database

   def limiter() : SignalLimiter
   def inFader( off: Long, len: Long ) : SignalFader
   def outFader( off: Long, len: Long ) : SignalFader

   def remove( spans: IIdxSeq[ Span ])( implicit tx: InTxn ) : FutureResult[ Unit ] = {
      val dbLen   = database.length
      val instrs  = spans.map { span =>
         val shrink     = shrinkMotion.step()
         val jitter     = secondsToFrames( jitterMotion.step() )
         val start0     = (span.start + jitter + shrink * (span.length/2)).toLong
         val stop0      = (span.stop + jitter - shrink * ((span.length+1)/2)).toLong
         val start      = max( 0L, min( dbLen, start0 ))
         val stop       = max( start, min( dbLen, stop0 ))
         val spanT      = Span( start, stop )

         val fade0      = secondsToFrames( fadeInMotion.step() )
         val fiPre      = min( spanT.start, fade0/2 )
         val foPost     = min( dbLen - spanT.stop, fade0/2 )

         val fiPost0    = fade0 - fiPre
         val foPre0     = fade0 - foPost
         val innerSum   = fiPost0 + foPre0

         val (fiPost, foPre) = if( innerSum <= spanT.length ) {
            (fiPost0, foPre0)
         } else {
            val scl     = spanT.length.toDouble / innerSum
            ((fiPost0 * scl).toLong, (foPre0 * scl).toLong)
         }

         val spanTFd    = Span( spanT.start - fiPre, spanT.stop + foPost )
//         val fadeIn     = fiPre + fiPost
//         val fadeOut    = foPre + foPost
         val fade       = min( fiPre + fiPost, foPre + foPost )

         RemovalInstruction( spanTFd, fade )
      }

      database.remove( instrs )
   }
}