/*
 *  Phrase.scala
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

import de.sciss.synth.proc.Proc
import impl.PhraseImpl
import java.io.File
import concurrent.stm.InTxn

object Phrase {
   def fromFile( file: File ) : Phrase = PhraseImpl.fromFile( file )
}
trait Phrase {
   def printFormat : String

   def length: Long
//   def player( implicit tx: Tx ) : Proc
   def asStrugatzkiInput( implicit tx: InTxn ) : FutureResult[ File ]

   def reader( implicit tx: Tx ) : FrameReader.Factory

//   /**
//    * Plays this phrase into a given diffusion proc
//    * (by creating a gen proc and connecting its output
//    * to the diffusion). The result represents the
//    * phrase
//    */
//   def playInto( diffusion: Proc )( implicit tx: Tx ) : FutureResult[ Unit ]
}