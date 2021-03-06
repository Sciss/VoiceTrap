/*
 *  FrameReader.scala
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

import de.sciss.synth.io.Frames
import impl.{FrameReaderFactoryImpl => Impl}
import java.io.File

object FrameReader {
   object Factory {
      def apply( file: File ) : Factory = Impl( file )
   }

   trait Factory {
      def open() : FrameReader
   }

}
trait FrameReader {
   /**
    * Reads a chunk of frames into a buffer, starting at the beginning of the buffer
    * and at a given frame offset into the reader.
    *
    * @param buf  the buffer to write out. data will be written starting from offset 0
    * @param off  the offset into the frame reader source. this must be advanced for successive chunks
    * @param len  the number of frames to read
    */
   def read( buf: Frames, off: Long, len: Int ) : Unit
   def close() : Unit
}