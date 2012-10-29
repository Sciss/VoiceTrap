/*
 *  FrameReaderFactoryImpl.scala
 *  (WritingMachine)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.synth.io.{AudioFile, Frames}

object FrameReaderFactoryImpl {
   def apply( file: File ) : FrameReader.Factory = new FrameReader.Factory {
      def open() : FrameReader = {
         val af = AudioFile.openRead( file )
         new ReaderImpl( af )
      }
   }

   private final class ReaderImpl( af: AudioFile ) extends FrameReader {
      override def toString = "FrameReader(" + af.file.getOrElse( "?" ) + ")"

      def read( buf: Frames, off: Long, len: Int ) {
         if( off != af.position ) { af.position = off }
         af.read( buf, 0, len )
      }

      def close() { af.close() }
   }
}