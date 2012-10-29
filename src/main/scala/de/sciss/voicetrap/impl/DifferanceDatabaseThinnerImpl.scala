/*
 *  DifferanceDatabaseThinner.scala
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

object DifferanceDatabaseThinnerImpl {
   def apply( database: Database ) : DifferanceDatabaseThinner = new DifferanceDatabaseThinnerImpl( database )
}
final class DifferanceDatabaseThinnerImpl private ( val database: Database ) extends AbstractDifferanceDatabaseThinner {
   import GraphemeUtil._

   val fadeInMotion : Motion  = Motion.exprand( 0.02, 2.0 )
   val fadeOutMotion : Motion = Motion.exprand( 0.03, 3.0 )
   val shrinkMotion : Motion  = Motion.linrand( 0.0, 1.0 )
   val jitterMotion : Motion  = Motion.linexp( Motion.walk( 0.0, 1.0, 0.1 ), 0.0, 1.0, 0.02222222, 5.0 )

   /**
    * Uses 1/3 pow which gives some -4 dB cross fade point, a bit weaker than equal power fade
    */
   def inFader( off: Long, len: Long ) : SignalFader = SignalFader( off, len, 0f, 1f, 0.66666f )

   /**
    * Uses 1/3 pow which gives some -4 dB cross fade point, a bit weaker than equal power fade
    */
   def outFader( off: Long, len: Long ) : SignalFader = SignalFader( off, len, 1f, 0f, 0.66666f )

   def limiter() : SignalLimiter = SignalLimiter( secondsToFrames( 0.100 ).toInt )
}