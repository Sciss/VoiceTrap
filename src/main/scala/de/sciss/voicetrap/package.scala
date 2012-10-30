/*
 *  package.scala
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

package de.sciss

import lucre.confluent.reactive.ConfluentReactive
import lucre.{DataOutput, DataInput, stm}
import synth.expr.ExprImplicits
import synth.io.AudioFileSpec
import synth.proc
import proc.Grapheme
import concurrent.stm.{InTxn, Txn}
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import annotation.elidable
import annotation.elidable._

package object voicetrap {
   type S               = ConfluentReactive

   object implicits extends ExprImplicits[ S ]

   type D               = stm.Durable
   type I               = stm.InMemory
   type Tx              = S#Tx
   type Acc             = S#Acc
   type ID              = S#ID
   type Var[ A ]        = S#Var[ A ]

   type Source[ A ]     = stm.Source[ Tx, A ]
   type Cursor          = lucre.confluent.Cursor[ S ]
   type ProcGroup       = proc.ProcGroup_.Modifiable[ S ]
   type Serializer[ A ] = stm.Serializer[ Tx, Acc, A ]

   type AudioArtifact   = Grapheme.Value.Audio
   type AudioSegment    = Grapheme.Segment.Audio
   type Transport       = proc.ProcTransport[ S ]
   type ArtifactStore   = proc.ArtifactStore[ S ]

   def ??? : Nothing = sys.error( "TODO" )

   def readSerVersion( in: DataInput, cookie: String, version: Int ) {
      val sb      = new StringBuilder( 3 )
      sb.append( in.readChar() )
      sb.append( in.readChar() )
      sb.append( in.readChar() )
      val cFound  = sb.toString()
      require( cFound == cookie, "Unexpected cookie, expected " + cookie + " but found " + cFound )
      val vFound = in.readUnsignedByte()
      require( vFound == version, "Expected serialized version " + version + ", but found " + vFound )
   }

   def writeSerVersion( out: DataOutput, cookie: String, version: Int ) {
      require( cookie.length == 3 )
      out.writeChar( cookie.charAt( 0 ))
      out.writeChar( cookie.charAt( 1 ))
      out.writeChar( cookie.charAt( 2 ))
      out.writeUnsignedByte( version )
   }

   def mapSerializer[ A, B ]( implicit entrySerializer: Serializer[ (A, B) ]) = stm.Serializer.map[ Tx, Acc, A, B ]

   implicit def artifactStoreSerializer   = proc.ArtifactStore.serializer[ S ]
   implicit def procGroupSerializer       = proc.ProcGroup_.Modifiable.serializer[ S ]
   implicit def cursorSerializer          = CursorSerializer

//   private lazy val pool : ExecutorService = {        // system wide scheduler
//      val res = Executors.newSingleThreadExecutor()
//      sys.addShutdownHook( shutdownThreadPool() )
//      res
//   }
//
//   private def shutdownThreadPool() {
//     pool.shutdown()
//   }

   private val fullDecouple = false

   def spawn( cursor: Cursor )( fun: Tx => Unit )( implicit tx: Tx ) {
//      Txn.afterCommit( _ => pool.submit( new Runnable {
//         def run() {
//            cursor.step { implicit tx => fun( tx )}
//         }
//      }))( tx.peer )
      Txn.afterCommit( _ => {
         val r = new Runnable {
            def run() {
               cursor.step { implicit tx => fun( tx )}
            }
         }

         if( fullDecouple ) {
            proc.SoundProcesses.pool.submit( r )
         } else {
            r.run()
         }

      })( tx.peer )
   }

   private lazy val logHeader = new SimpleDateFormat( "[d MMM yyyy, HH:mm''ss.SSS] 'voice' - ", Locale.US )
   var showLog = true

   @elidable(CONFIG) private[voicetrap] def log( what: => String ) {
      if( showLog ) Console.out.println( logHeader.format( new Date() ) + what )
   }

   def atom[ A ]( info: => String )( fun: InTxn => A ) : A = {
      import concurrent.stm.atomic
      atomic { tx =>
         log( "atomic: " + info )
         fun( tx )
      }
   }

   def audioFileSpec( path: String ) : AudioFileSpec = AudioFileCache.spec( path )
}