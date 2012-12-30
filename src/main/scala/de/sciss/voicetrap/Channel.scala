/*
 *  Channel.scala
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

import impl.{ChannelImpl => Impl}
import de.sciss.lucre.Writable
import de.sciss.synth.proc

object Channel {
   implicit def serializer: Serializer[ Channel ] = Impl.serializer

   def apply( row: Int, column: Int, group: ProcGroup )( implicit tx: Tx ) : Channel = Impl( row, column, group )
}
trait Channel extends Writable {
   def row: Int
   def column: Int

   def hiddenLayer : AudioArtifact
//   def cursor( implicit tx: Tx ): Cursor

//   def refresh( implicit tx: Tx ) : Channel

   def insert( segm: AudioSegment )( implicit tx: Tx ) : Unit

   def removeAt(   time: Long )( implicit tx: Tx ) : Unit
   def removeFrom( time: Long )( implicit tx: Tx ) : Unit

   def nextSearch( loop: Long, iter: Int, iterZeroTime: Long, document: Document, auralSystem: proc.AuralSystem[ S ],
                   server: proc.Server, transportOption: Option[ Transport ])( implicit tx: Tx, cursor: Cursor ) : Unit

   // ---- algorithm ----

//   def start( document: Document, auralSystem: AuralSystem[ S ])( implicit tx: Tx ) : Unit
   def start( document: Document, server: proc.Server, auralSystem: proc.AuralSystem[ S ])( implicit tx: Tx, cursor: Cursor ) : Unit
   def stop()( implicit tx: Tx ) : Unit

//   def fork()( implicit tx: Tx ) : Unit
}