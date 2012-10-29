/*
 *  SignalFaderImpl.scala
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

object SignalFaderImpl {
   def apply( off: Long, len: Long, start: Float, stop: Float, pow: Float = 1f ) : SignalFader = {
      val lvl = if( pow == 1f ) new LinLevel( start, stop ) else new PowLevel( start, stop, pow )
      new SignalFaderImpl( off, len, lvl )
   }

   sealed trait Level {
      def apply( w: Float ) : Float
   }
   private final class LinLevel( start: Float, stop: Float ) extends Level {
      def apply( w: Float ) : Float = (1 - w) * start + w * stop
   }
   private final class PowLevel( start: Float, stop: Float, pow: Float ) extends Level {
      def apply( w: Float ) : Float = math.pow( 1 - w, pow ).toFloat * start + math.pow( w, pow ).toFloat * stop
   }
}
final class SignalFaderImpl private ( off: Long, len: Long, lvl: SignalFaderImpl.Level )
extends SignalFader {
   private var pr = -off

   def process( in: Array[ Float ], inOff: Int, out: Array[ Float ], outOff: Int, len: Int ) {
      var i = 0
      val _pr = pr
      while( i < len ) {
         val w = math.max( 0.0, math.min( 1.0, (_pr + i).toDouble / len)).toFloat
         out( outOff + i ) = lvl( w ) * in( inOff + i )
         i += 1
      }
      pr += len
   }
}