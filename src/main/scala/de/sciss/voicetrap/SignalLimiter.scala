/*
 *  SignalLimiter.scala
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

//import impl.{SignalLimiterImpl => Impl}

import de.sciss.voicetrap.impl.{DummyLimiterImpl => Impl} // XXX

object SignalLimiter {
  /**
   * Default ceiling is -0.2 dB.
   */
  def apply(lookAhead: Int, ceil: Float = 0.977f): SignalLimiter = Impl(lookAhead, ceil)
}

trait SignalLimiter {
  def process(in: Array[Float], inOff: Int, out: Array[Float], outOff: Int, len: Int): Int

  def latency: Int
  //   def flush( out: Array[ Float ], outOff: Int, len: Int ) : Int
}