/*
 *  DocumentImpl.scala
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
package impl

import de.sciss.lucre.{stm, DataOutput, DataInput}
import de.sciss.synth
import synth.proc
import proc.AuralSystem
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.text.SimpleDateFormat
import java.util.{Date, Locale}

object DocumentImpl {
   private final val SER_VERSION = 1

   import VoiceTrap.{numRows, numColumns, matrixSize}

   private lazy val df = new SimpleDateFormat( "HH:mm''ss.SSS", Locale.US )

   implicit object serializer extends stm.Serializer[ Tx, Acc, Document ] {
      def write( v: Document, out: DataOutput ) { v.write( out )}
      def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Document = {
         log( "Read document" )
         readSerVersion( in, "doc", SER_VERSION )
         val id               = tx.readID( in, access )
         val cursor           = tx.readCursor( in, access )
         val num              = in.readInt()
         require( num == matrixSize, "Changed matrix size. Stored " + num + " but configured " + matrixSize )
         val chanCursorVars   = IIdxSeq.fill( num ) {
            tx.readVar[ Cursor ]( id, in )
         }
//         val chanVars         = IIdxSeq.tabulate( num ) { i =>
////            val row     = i / numColumns
////            val column  = i % numColumns
//            tx.readVar[ Channel ]( id, in )
//         }

         val channels      = IIdxSeq.fill( num )( Channel.serializer.read( in, access ))
         val chanHandles   = channels.map( tx.newHandle( _ ))

         val artifactStoreVar = tx.readVar[ ArtifactStore ]( id, in ) // .read[ S ]( in, access )
         new Impl( id, cursor, chanCursorVars, channels, chanHandles, artifactStoreVar )
      }
   }

   def apply()( implicit tx: Tx ) : Document = {
      val id               = tx.newID()
      val cursor           = tx.newCursor()
//      val groupVar   = tx.newVar( id, proc.ProcGroup_.Modifiable[ S ])
      val group            = proc.ProcGroup_.Modifiable[ S ]
      val chanCursors      = IIdxSeq.fill( matrixSize )( tx.newCursor() )
      val chanCursorVars   = chanCursors.map( tx.newVar( id, _ ))
      val channels         = IIdxSeq.tabulate( matrixSize ) { i =>
         val row     = i / numColumns
         val column  = i % numColumns
         Channel( row, column, group )
//         tx.newVar( id, )
      }
      val chanHandles      = channels.map( tx.newHandle( _ ))
      val artifactStoreVar = tx.newVar( id, proc.ArtifactStore[ S ]( VoiceTrap.artifactDirectory ))
      new Impl( id, cursor, chanCursorVars, channels, chanHandles, artifactStoreVar )
   }

   private final class Impl( val id: ID, val cursor: Cursor,
                             chanCursorVars: IIdxSeq[ Var[ Cursor ]],
                             channelsStale: IIdxSeq[ Channel ],
                             chanHandles: IIdxSeq[ Source[ Channel ]],
                             val artifactStoreVar : Var[ ArtifactStore ])
   extends Document {
      doc =>

      override def toString = "Document"

//      def group( implicit tx: Tx ) : ProcGroup = groupVar.get
      def artifactStore( implicit tx: Tx ) : ArtifactStore = artifactStoreVar.get

//      /**
//       * Fork random range bounds in seconds
//       */
//      val minMaxFork = (60.0, 240.0)

      /**
       * Wrapping duration in seconds for the performance time
       */
      val pDur = 60.0

      def start( server: proc.Server, auralSystem: AuralSystem[ S ])( implicit tx: Tx ) {
         for( row <- 0 until numRows; column <- 0 until numColumns ) {
            withChannel( row, column, jumpBack = None ) { case (tx1, csr, chan) =>
               chan.start( doc, server, auralSystem )( tx1, csr )
            }
         }
//         channels.valuesIterator.foreach( _.start( doc, auralSystem ))
      }

      def withChannel[ A ]( row: Int, column: Int, jumpBack: Option[ Long ])( fun: (Tx, Cursor, Channel) => Unit )( implicit tx: Tx ) {
         val i    = row * numColumns + column
         val csr  = chanCursorVars( i ).get

         jumpBack.foreach { timeStamp =>
            log( "::::::::::::::::::" )
            log( "jump back " + (row+1) + "_" + (column+1) + " to " + df.format( new Date( timeStamp )))
            log( "::::::::::::::::::" )
         }

         spawn( csr, jumpBack ) { implicit tx =>
            val ch = chanHandles( i ).get
            fun( tx, csr, ch )
         }
      }

      def stop()( implicit tx: Tx ) {
         for( i <- 0 until matrixSize ) {
            val csr = chanCursorVars( i ).get
            spawn( csr ) { implicit tx =>
               val ch = chanHandles( i ).get
               ch.stop()
            }
         }
//         channels.valuesIterator.foreach( _.stop() )
      }

      def write( out: DataOutput ) {
         writeSerVersion( out, "doc", SER_VERSION )
         id.write( out )
         cursor.write( out )
//         mapSerializer[ (Int, Int), Channel ].write( channels, out )
         out.writeInt( matrixSize )
         chanCursorVars.foreach( _.write( out ))
         channelsStale.foreach( _.write( out ))
         artifactStoreVar.write( out )
      }
   }
}