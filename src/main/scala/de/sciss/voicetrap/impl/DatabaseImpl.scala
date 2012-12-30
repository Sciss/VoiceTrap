/*
 *  DatabaseImpl.scala
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

import java.io.{IOException, FileFilter, File}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import de.sciss.lucre.bitemp.Span
import concurrent.stm.{InTxn, Ref}

object DatabaseImpl {
   private val normName    = "feat_norms.aif"
   private val dirPrefix   = "database"
//   private val xmlName     = "grapheme.xml"
   private val audioName   = "grapheme.aif"

//   private val identifier  = "database-impl"

   def apply( name: String, dir: File ) : Database = {
      val normFile = new File( dir, normName )
      require( normFile.isFile, "Missing normalization file at " + normFile )

      val arr = dir.listFiles( new FileFilter {
         def accept( f: File ) = f.isDirectory && f.getName.startsWith( dirPrefix )
      })
      val subs = (if( arr == null ) Nil else arr.toList).sortBy( f => -f.lastModified() ) // newest first

      // left-overs from aborted apps
      subs.drop( 1 ).foreach( deleteDir( _ ))

      val /*(*/spec /*, extr)*/ = subs.headOption match {
         case Some( sub ) =>
//            val fExtr   = new File( sub, xmlName )
//            val fNorm   = new File( sub, normName )
//            if( fExtr.isFile && fNorm.isFile ) {
            val fAudio     = new File( sub, audioName )
            if( fAudio.isFile ) {
               try {
//                  val extr       = FeatureExtraction.Settings.fromXMLFile( fExtr )
//                  val spec       = AudioFile.readSpec( extr.audioInput )
                  val spec       = AudioFile.readSpec( fAudio )
                  Some( (fAudio, spec) )
//                  val normSpec   = AudioFile.readSpec( fNorm )
//                  if( normSpec.numFrames == 2 ) {  // make sure this is there and was fully written
//                     (Some( (extr.audioInput, spec) ), Some( fExtr ))
//                  } else {
//                     (None, None)
//                  }
               } catch {
                  case e: IOException =>
                     e.printStackTrace()
                     None // (None, None)
               }
            } else None // (None, None)

         case None => None // (None, None)
      }

      new DatabaseImpl( name, dir, normFile, State( spec, None /* extr */))
   }

   /*
    * Assumes the directory is empty or contains only ordinary files.
    */
   private def deleteDir( dir: File ) : Boolean = {
      val arr  = dir.listFiles()
      val sub  = if( arr == null ) Nil else arr.toList
      sub.forall( _.delete() ) && dir.delete()
   }

   private def copyAudioFile( name: String, source: File, target: File )( implicit tx: InTxn ) : FutureResult[ Unit ] = {
      import GraphemeUtil._

      threadFuture( name + " : copy file" ) {
         val afSrc = AudioFile.openRead( source )
         try {
            val afTgt = AudioFile.openWrite( target, afSrc.spec )
            try {
               afSrc.copyTo( afTgt, afSrc.numFrames )
               () // oops... old stuff, copyTo should return Unit these days
            } finally {
               afTgt.close()
            }
         } finally {
            afSrc.close()
         }
      }
   }

//   final case class Entry( extr: FeatureExtraction.Settings, spec: AudioFileSpec )
   final case class State( spec: Option[ (File, AudioFileSpec) ], extr: Option[ File ])
}
class DatabaseImpl private ( val identifier: String, dir: File, normFile: File, state0: DatabaseImpl.State )
extends Database /* AbstractDatabase */ with ExtractionImpl {
   import GraphemeUtil._
   import DatabaseImpl._

//   def identifier = DatabaseImpl.identifier

//   private val extrRef        = Ref( extr0 )
//   private val specRef        = Ref( spec0 )
   private val stateRef       = Ref( state0 )

   override def toString = "Database@" + hashCode.toHexString

//   private val folderRef      = Ref( grapheme0.flatMap( _.extr.metaOutput ).map( _.getParentFile )
//      .getOrElse( createDir( dir )))

//   val removalFadeMotion      = Motion.exprand( 0.100, 1.000 )
//   val removalSpectralMotion  = Motion.linrand( 0.20, 0.80 )
//   val removalMarginMotion    = Motion.exprand( 0.250, 2.500 )
//
//   def performRemovals( instrs: IIdxSeq[ RemovalInstruction ])( implicit tx: InTxn ) : FutureResult[ Unit ] = {
//      sys.error( "TODO" )
//   }
//
//   def bestRemoval( span: Span, margin: Long, weight: Double, fade: Long )( implicit tx: InTxn ) : RemovalInstruction =
//      sys.error( "TODO" )

   def append( appFile: File, offset: Long, length: Long )( implicit tx: InTxn ) : FutureResult[ Unit ] = {
      val oldFileO  = stateRef().spec.map( _._1 )
      threadFuture( identifier + " : append " + formatSeconds( framesToSeconds( length ))) {
//         atomic( "Database append tx" ) { implicit tx =>
            appendBody( oldFileO, appFile, offset, length )
//         }
      }
   }

   def remove( instrs: IIdxSeq[ RemovalInstruction ])( implicit tx: InTxn ) : FutureResult[ Unit ] = {
      val len        = length
      val filtered   = instrs.filter( i => i.span.nonEmpty && i.span.start >= 0 && i.span.stop <= len )
      val sorted     = filtered.sortBy( _.span.start )

      val merged     = {
         var pred       = RemovalInstruction( Span( -1L, -1L ), 0L )
         var res        = IIdxSeq.empty[ RemovalInstruction ]
         sorted.foreach { succ =>
            pred.merge( succ ) match {
               case Some( m ) =>
                  pred = m
               case None =>
                  res :+= pred
                  pred = succ
            }
         }
         if( pred.span.nonEmpty ) res :+= pred
         res
      }

      val oldFileO  = stateRef().spec.map( _._1 )
      oldFileO match {
         case Some( f ) =>
            threadFuture( identifier + " : remove" ) {
               // add a dummy instruction to the end, so we can easier traverse the list
               removalBody( f, merged :+ RemovalInstruction( Span( len, len ), 0L ))
            }
         case None => futureOf( identifier + " no remove needed", () )
      }
   }

   private def updateLoop( fun: AudioFile => Unit ) {
//      try {
         val sub     = createDir( dir, dirPrefix )
         val fNew    = new File( sub, audioName )
         val afNew   = openMonoWrite( fNew )
         try {
            fun( afNew )
            afNew.close()
//            atomic( identifier + " : update finalize" ) { tx =>
               val oldState   = stateRef.single.swap( State( Some( (fNew, afNew.spec) ), None )) // ( tx )
               val oldFileO2  = oldState.spec.map( _._1 )
//               tx.afterCommit { _ =>
                  oldFileO2.foreach { fOld2 =>
                     deleteDir( fOld2.getParentFile )
                  }
//               }
//            }
         } finally {
            if( afNew.isOpen ) afNew.close()
         }

//      } catch {   // FFF
//         case e =>
//            println( "Database update - Ooops, should handle exceptions" )
//            e.printStackTrace()
//      }
   }

   private def removalBody( fOld: File, entries: IIdxSeq[ RemovalInstruction ]) {
      import DSP._

      updateLoop { afNew =>
         var shrink  = 0L
         var pos     = 0L
         var written = 0L
         val lim     = SignalLimiter( secondsToFrames( 0.1 ).toInt )

         val afOld   = AudioFile.openRead( fOld )
         try {
            val inBuf   = afOld.buffer( 8192 )
            val fInBuf  = inBuf( 0 )
            val outBuf  = afOld.buffer( 8192 )
            val fOutBuf = outBuf( 0 )
            entries.foreach { entry =>
               val start   = entry.span.start - shrink
               // copy
               while( pos < start ) {
                  val chunkLen   = min( start - pos, 8192 ).toInt
                  afOld.read( inBuf, 0, chunkLen )
                  val chunkLen2 = lim.process( fInBuf, 0, fOutBuf, 0, chunkLen )
                  afNew.write( outBuf, 0, chunkLen2 )
                  written += chunkLen2
                  pos += chunkLen
               }
               // fade
               val fdLen   = entry.fade
               if( fdLen > 0 ) {
                  var done    = 0L
                  val fo      = SignalFader( 0L, fdLen, 1f, 0f, 0.6666f )
                  val fi      = SignalFader( 0L, fdLen, 0f, 1f, 0.6666f )
                  while( done < fdLen ) {
                     val chunkLen = min( fdLen - done, 8192 ).toInt
                     afOld.seek( entry.span.start + done )
                     afOld.read( inBuf, 0, chunkLen )
                     afOld.seek( entry.span.stop - fdLen + done )
                     afOld.read( outBuf, 0, chunkLen )
                     fo.process( fInBuf, 0, fInBuf, 0, chunkLen )
                     fi.process( fOutBuf, 0, fOutBuf, 0, chunkLen )
                     add( fOutBuf, 0, fInBuf, 0, chunkLen )
                     val chunkLen2 = lim.process( fInBuf, 0, fOutBuf, 0, chunkLen )
                     afNew.write( outBuf, 0, chunkLen2 )
                     written += chunkLen2
                     done += chunkLen
                  }
                  pos += entry.fade
               }
               shrink += entry.span.length - entry.fade
            }
            clear( fInBuf, 0, 8192 )
            while( written < pos ) {
               val chunkLen   = math.min( pos - written, 8192 ).toInt
               val chunkLen2  = lim.process( fInBuf, 0, fOutBuf, 0, chunkLen )
               afNew.write( outBuf, 0, chunkLen2 )
               written += chunkLen2
            }
         } finally {
            afOld.close()
         }
      }
   }

   private def appendBody( oldFileO: Option[ File ], appFile: File, off0: Long, len0: Long ) {
      import DSP._

      updateLoop { afNew =>
         val afApp = AudioFile.openRead( appFile )
//println( "app file = " + appFile + "; numFrames = " + afApp.numFrames + "; off = " + offset + " ; len = " + len )
         try {
            // this is important: DiskOut writes in blocks, and therefore the actual
            // number of frames in afApp may be a bit smaller than the len0 argument!
            val off  = min( off0, afApp.numFrames )
            val len = min( len0, afApp.numFrames - off )

            if( off > 0 ) afApp.seek( off )
            val fdLen = oldFileO.map( fOld => {
               val afOld   = AudioFile.openRead( fOld )
               try {
                  require( afOld.numChannels == afApp.numChannels, identifier + " : append - channel mismatch" )
                  val _fdLen = min( afOld.numFrames, len, secondsToFrames( 0.1 )).toInt
                  afOld.copyTo( afNew, afOld.numFrames - _fdLen )
                  if( _fdLen > 0 ) {
                     val fo      = SignalFader( 0L, _fdLen, 1f, 0f )  // make it linear, so we don't need a limiter
                     val foBuf   = afOld.buffer( _fdLen )
                     afOld.read( foBuf )
                     fo.process( foBuf( 0 ), 0, foBuf( 0 ), 0, _fdLen )
                     val fi      = SignalFader( 0L, _fdLen, 0f, 1f )
                     val fiBuf   = afApp.buffer( _fdLen )
                     afApp.read( fiBuf )
                     fi.process( fiBuf( 0 ), 0, fiBuf( 0 ), 0, _fdLen )
                     add( fiBuf( 0 ), 0, foBuf( 0 ), 0, _fdLen )
                     afNew.write( fiBuf )
                  }
                  _fdLen
               } finally {
                  afOld.close()
               }
            }).getOrElse( 0 )
            afApp.copyTo( afNew, len - fdLen )
         } finally {
            afApp.close()
         }
      }
   }

   def length( implicit tx: InTxn ) : Long   = stateRef().spec.map( _._2.numFrames ).getOrElse( 0L )
   def lengthSingle : Long            = stateRef.single().spec.map( _._2.numFrames ).getOrElse( 0L )

   def reader : FrameReader.Factory = {
      val f = stateRef.single().spec.map( _._1 ).getOrElse( sys.error( identifier + " : contains no file" ))
      FrameReader.Factory( f )
   }

   def asStrugatziDatabase( implicit tx: InTxn ) : FutureResult[ File ] = {
      val state = stateRef()
      state.extr match {
         case Some( meta ) => futureOf( identifier + " meta ready", meta.getParentFile )
         case None =>
            val audioInput = state.spec.map( _._1 ).getOrElse( sys.error( identifier + " : contains no file" ))
            val sub        = audioInput.getParentFile
            copyAudioFile( identifier, normFile, new File( sub, normName )).flatMap( identifier + " extract+update" ) { _ =>
               extractAndUpdate( audioInput, sub )
            }
      }
   }

   private def extractAndUpdate( audioInput: File, sub: File ) : FutureResult[ File ] = {
//      atomic( identifier + " : extract" ) { implicit tx =>
         extract( audioInput, Some( sub ), keep = true ).mapSuccess( identifier + " update state" ) { meta =>
            assert( meta.getParentFile == sub )
            updateState( meta )
            sub
         }
//      }
   }

   private def updateState( meta: File ) {
//      atomic( identifier + " : cache feature extraction" ) { implicit tx =>
         stateRef.single.transform( _.copy( extr = Some( meta )))
//      }
   }
}