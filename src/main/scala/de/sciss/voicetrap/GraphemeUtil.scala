/*
 *  GraphemeUtil.scala
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

import java.io.File
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
import de.sciss.lucre.bitemp.{SpanLike, Span}
import concurrent.stm.{Txn, InTxn}

import VoiceTrap.sampleRate

object GraphemeUtil {

//   var logTransactions        = true
//   /**
//    * Important: there seem to be problems with communications between
//    * WritingMachine and scsynth when using `sys.props( "java.io.tmpdir" )`
//    * which gives you something terrible as `/var/folders/M9/M9W+ucwpFzaqGpC2mu3KZq0sUCc/-Tmp-/`
//    */
//   var tmpDir                 = new File( "/tmp" )

   val seed                   = System.currentTimeMillis()  // 0L
   private val rng            = new util.Random( seed )

   val deleteTempFilesOnExit  = true   // false

//   def logNoTx( text: => String ) {
//      if( logTransactions ) logPrint( text )
//   }
//
//   def logTx( text: => String )( implicit tx: Tx ) {
//      if( logTransactions ) tx.afterCommit( _ => logPrint( text ))
//   }
//
//   private def logPrint( text: String ) {
//      println( timeString() + " " + text )
//   }

   def fileNameWithoutExtension( f: File ) : String = {
      val n = f.getName
      val i = n.lastIndexOf( '.' )
      if( i < 0 ) n else n.substring( 0, i )
   }

   def formatSpan( span: SpanLike ) : String = {
      val sb   = new StringBuilder( 24 )
      sb.append( '[' )
      sb.append( span match {
         case s: Span.HasStart => formatSeconds( framesToSeconds( s.start ))
         case _ => "?"
      })
      sb.append( '-' )
      sb.append( span match {
         case s: Span.HasStop => formatSeconds( framesToSeconds( s.stop ))
         case _ => "?"
      })
      sb.append( ']' )
      sb.toString()
   }

   def formatPercent( d: Double ) : String = {
      val pm   = (d * 1000).toInt
      val post = pm % 10
      val pre  = pm / 10
      val sb   = new StringBuilder( 8 )
      sb.append( pre )
      sb.append( '.' )
      sb.append( post )
      sb.append( '%' )
      sb.toString()
   }

   def timeString() = (new java.util.Date()).toString

   def formatSeconds( seconds: Double ) : String = {
      val millisR    = (seconds * 1000).toInt
      val sb         = new StringBuilder( 10 )
      val secsR      = millisR / 1000
      val millis     = millisR % 1000
      val mins       = secsR / 60
      val secs       = secsR % 60
      if( mins > 0 ) {
         sb.append( mins )
         sb.append( ':' )
         if( secs < 10 ) {
            sb.append( '0' )
         }
      }
      sb.append( secs )
      sb.append( '.' )
      if( millis < 10 ) {
         sb.append( '0' )
      }
      if( millis < 100 ) {
         sb.append( '0' )
      }
      sb.append( millis )
      sb.append( 's' )
      sb.toString()
   }

//   def random( implicit tx: Tx ) : Double = math.random // XXX
   def random( implicit tx: InTxn ) : Double = rng.nextDouble()
//   def sampleRate : Double = 44100.0

   def random( top: Int )( implicit tx: InTxn ) : Int = (random * top).toInt

   def secondsToFrames( secs: Double ) : Long   = (secs * sampleRate + 0.5).toLong
   def framesToSeconds( frames: Long ) : Double = frames / sampleRate

   def max( i: Int, is: Int* ) : Int = is.foldLeft( i )( _ max _ )
   def max( n: Long, ns: Long* ) : Long = ns.foldLeft( n )( _ max _ )
   def max( d: Double, ds: Double* ) : Double = ds.foldLeft( d )( _ max _ )
   def min( i: Int, is: Int* ) : Int = is.foldLeft( i )( _ min _ )
   def min( n: Long, ns: Long* ) : Long = ns.foldLeft( n )( _ min _ )
   def min( d: Double, ds: Double* ) : Double = ds.foldLeft( d )( _ min _ )

   def openMonoWrite( f: File ) : AudioFile =
      AudioFile.openWrite( f, AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, 1, sampleRate ))

//   def strugatzkiDatabase = WritingMachine.strugatzkiDatabase
//   def databaseDir = databaseDirectory

   def createTempFile( suffix: String, dir: Option[ File ], keep: Boolean ) : File = {
      val res = File.createTempFile( "grapheme", suffix, dir.getOrElse( VoiceTrap.temporaryDirectory ))
//if( keep ) println( "Created tmp file : " + res )
      if( !keep && deleteTempFilesOnExit ) res.deleteOnExit()
      res
   }

   def createDir( parent: File, prefix: String ) : File = {
      val f = File.createTempFile( prefix, "", parent )
      f.delete()
      require( f.mkdir(), "Could not create directory : " + f )
      f
   }

   def futureOf[ A ]( name: String, value: A ) : FutureResult[ A ] = FutureResult.nowSucceed( name, value )

   def threadFuture[ A ]( name: String )( code: => FutureResult.Result[ A ])( implicit tx: InTxn ) : FutureResult[ A ] = {
//      requireTxnThread()
      val ev = FutureResult.event[ A ]( name )
      threadTxn( name ) {
         log( "threadFuture started : " + name )
         val res = code
         submit( ev.set( res ))
//         ev.set( try {
//            code
//         } catch {
//            case e: Throwable => FutureResult.Failure( e )
//         })
      } { e =>
         submit( ev.fail( e ))
      }
      ev
   }

   def warnToDo( what: => String ) {
      log( "+++MISSING+++ " + what )
   }

   def threadTxn( name: String )( code: => Unit )( failure: Throwable => Unit )( implicit tx: InTxn ) {
//      requireTxnThread()
      Txn.afterCommit { _ =>
         thread( name ) {
            try( code ) catch {
               case e: Throwable =>
                  e.printStackTrace()
                  failure( e )
            }
         }
      }
      Txn.afterRollback {
         case Txn.RolledBack( Txn.UncaughtExceptionCause( e )) =>
            e.printStackTrace()
            failure( e )
         case _ =>
      }
   }

   def thread( name: String )( code: => Unit ) {
      requireNotInTxn()
      new Thread( name ) {
         log( name )
         start()
         override def run() {
            code
         }
      }
   }

//   def threadAtomic( info: => String )( fun: Tx => Unit ) {
//      thread( info ) {
//         atomic( info )( fun( _ ))
//      }
//   }
}