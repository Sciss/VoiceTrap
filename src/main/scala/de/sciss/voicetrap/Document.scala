/*
 *  Document.scala
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

//import collection.immutable.{IndexedSeq => IIdxSeq}
import impl.{DocumentImpl => Impl}
import de.sciss.lucre.Writable
import de.sciss.synth.proc
import proc.AuralSystem

object Document {
  implicit def serializer: Serializer[Document] = Impl.serializer

  def apply()(implicit tx: Tx): Document = {
    log("New document")
    Impl()
  }
}

trait Document extends Writable {
  def cursor: Cursor

  def artifactStore(implicit tx: Tx): ArtifactStore

  //   /**
  //    * Map from (row, column) to channel
  //    */
  //   def channels : Map[ (Int, Int), Channel ] // Var[ Map[ (Int, Int), Channel ]]

  def withChannel[A](row: Int, column: Int, jumpBack: Option[Long])(fun: (Tx, Cursor, Channel) => Unit)
                    (implicit tx: Tx): Unit

  //   /**
  //    * Fork random range bounds in seconds
  //    */
  //   def minMaxFork : (Double, Double) // Var[ (Double, Double) ]

  //   /**
  //    * One group is shared across channels, which merely
  //    * differ in their cursor access...
  //    */
  //   def group( implicit tx: Tx ) : ProcGroup // Source[ ProcGroup ]

  /** Wrapping duration in seconds for the performance time. */
  def pDur: Double // Var[ Double ]

  def start(server: proc.Server, auralSystem: AuralSystem[S])(implicit tx: Tx): Unit

  def stop()(implicit tx: Tx): Unit
}