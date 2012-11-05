/*
 *  SearchStepAlgorithm.scala
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

import de.sciss.lucre.bitemp.Span
import de.sciss.synth.proc.{RichServer, Artifact, Scan, Grapheme}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec, AudioFile}

object SearchStepAlgorithm {
   case class Chunk( segm: AudioSegment, fadeIn: SignalFader, fadeOut: SignalFader ) {
      def open()( implicit artifactStore: ArtifactStore ) : Open = Open(
         af     = AudioFile.openRead( segm.value.artifact.toFile ),
         stop   = stop,
         fadeIn = fadeIn, fadeOut = fadeOut
      )

      def stop : Long = segm.span match {
         case Span( _, st ) => st
         case _ => segm.span.start
      }
   }
   case class Open( af: AudioFile, stop: Long, fadeIn: SignalFader, fadeOut: SignalFader )

   def apply( channel: Channel, server: RichServer, span: Span, group: ProcGroup, hidden: AudioArtifact )
            ( implicit tx: Tx, artifactStore: ArtifactStore ) : FutureResult[ AudioArtifact ] = {
      // first, calculate all the 'cutted' audio segments within the given target span
      val posSegms = group.intersect( span ).toIndexedSeq.flatMap { case (sp, seq) =>
         seq.flatMap { timed =>
            val p = timed.value
            p.scans.get( "sig" ).flatMap( _.source ) match {
               case Some( Scan.Link.Grapheme( peer )) =>
                  peer.segment( span.start ) match {
                     case Some( segm @ Grapheme.Segment.Audio( _, _ )) =>
                        val res0 = intersect( segm, span )
                        val res1: Option[ AudioSegment ] = if( VoiceTrap.shrinkAmount > 0.0 ) {
                           res0.flatMap { segm =>
                              segm.span match {
                                 case sp @ Span( _, _ ) =>
                                    val len     = ((sp.length * VoiceTrap.shrinkAmount)/2).toLong
                                    if( len > 0 ) {
                                       val shrink  = Span( sp.start + len, sp.stop - len )
                                       intersect( segm, shrink )
                                    } else {
                                       Some( segm )
                                    }
                                 case _ => Some( segm )
                              }
                           }
                        } else res0
                        res1

                     case _ => None
                  }
               case _ => None
            }
         }
      }

      // next, add spans for the silent parts
      // (retain those that are sufficiently long)
      val negSpans = posSegms.foldLeft( IIdxSeq( span )) { case (res, segm) =>
         res.flatMap { sp => sp.subtract( segm.span ).filter( _.length >= 44100L )}
      }

      // ... and convert them into segments from the hidden layer
      val hiddenFrames  = hidden.spec.numFrames
      val hiddenLen     = hiddenFrames - hidden.offset
      val sysOffset     = ((System.currentTimeMillis() - VoiceTrap.startTime) / 1000.0 * VoiceTrap.sampleRate).toLong
      val negSegms = negSpans.flatMap { sp =>
         var hSegm   = IIdxSeq.empty[ AudioSegment ]
//         var afStart = (sp.start % hiddenLen) + hidden.offset
         var afStart = (sysOffset % hiddenLen) + hidden.offset
         var timOff  = sp.start
         val timStop = sp.stop
         while( timOff < timStop ) {
            val afStop     = math.min( hiddenFrames, afStart + (timStop - timOff) )
            val newOffset  = afStart
            val value      = hidden.copy( offset = newOffset )
            val afSpan     = Span( afStart, afStop )
            val chunkLen   = afSpan.length
            if( chunkLen >= 44100L ) {
               val segm    = Grapheme.Segment.Audio( Span( timOff, timOff + chunkLen ), value )
               hSegm     :+= segm
            }
            timOff        += chunkLen
            afStart        = (afStop - hidden.offset) % hiddenLen + hidden.offset
         }
         hSegm
      }

      val allSegms   = posSegms ++ negSegms
      val allChunks  = allSegms.map { segm =>
         val (fLen, spLen) = segm.span match {
            case sp @ Span( _, _ ) => math.min( 4410L, sp.length / 2 ) -> sp.length
            case _ => 0L -> 0L
         }
         val fIn  = SignalFader( off = 0L, len = fLen, start = (if( fLen == 0L ) 1f else 0f), stop = 1f, pow = 1f )
         val fOut = SignalFader( off = spLen - fLen, len = fLen, start = 1f, stop = (if( fLen == 0L ) 1f else 0f), pow = 1f )
         Chunk( segm, fIn, fOut )
      }

      val chanDB = VoiceTrap.databases( channel.row )( channel.column )

      val futFill = chanDB.filler.perform( server )

      val futPhrase = futFill.mapSuccess( "bounce " + channel ) { _ =>
         val phrase = bounce( allChunks )
         FutureResult.Success( phrase )
      }

      val queryMsg = "query " + channel
      val futQuery = futPhrase.flatMapSuccess( queryMsg ) { phrase =>
         atom( queryMsg ) { itx =>
            val q = chanDB.query
            q.find( phrase )( itx )
         }
      }
      val thinMsg = "thin " + channel
      val futArtifact = futQuery.flatMapSuccess( thinMsg ) { m =>
         val artifact = matchToValue( m )
         val remSpans = if( (VoiceTrap.drainProbability) > 0.0 && util.Random.nextDouble() < VoiceTrap.drainProbability ) {
            IIdxSeq( Span( 0, chanDB.database.lengthSingle / 3 ))
         } else IIdxSeq( m.span )

         val futUnit  = atom( thinMsg )( itx => chanDB.thinner.remove( remSpans )( itx ))
         futUnit.mapSuccess( thinMsg )( _ => artifact )
      }

      futArtifact
   }

   def matchToValue( m: DifferanceDatabaseQuery.Match ) : AudioArtifact = {
      val spanLen = m.span.length
      val outF    = GraphemeUtil.createTempFile( suffix = ".aif", dir = Some( VoiceTrap.artifactDirectory ), keep = true )
      val afSpec  = AudioFileSpec( fileType = AudioFileType.AIFF, sampleFormat = SampleFormat.Float,
                                   numChannels = 1, sampleRate = VoiceTrap.sampleRate, numFrames = spanLen )
      val afOut   = AudioFile.openWrite( outF, afSpec )
      val frame   = m.database.reader.open()
      val fadeLen = math.min( 22050L, spanLen / 2 )
      val fadeIn  = SignalFader( off = 0L, len = fadeLen, start = 0f, stop = 1f, pow = 1f )
      val fadeOut = SignalFader( off = spanLen - fadeLen, len = fadeLen, start = 1f, stop = 0f, pow = 1f )
      val boost   = SignalFader( off = 0L, len = spanLen, start = m.boostIn, stop = m.boostOut, pow = 1f )
      val buf     = afOut.buffer( 8192 )
      val bufCh   = buf( 0 )
      var off     = 0L
      while( off < spanLen ) {
         val chunkLen   = math.min( 8192, spanLen - off ).toInt
         frame.read( buf, off, chunkLen )
         fadeIn.process(  bufCh, 0, bufCh, 0, chunkLen )
         fadeOut.process( bufCh, 0, bufCh, 0, chunkLen )
         boost.process(   bufCh, 0, bufCh, 0, chunkLen )
         afOut.write( buf, 0, chunkLen )
         off += chunkLen
      }
      frame.close()
      afOut.close()
      val name       = outF.getName
      val artifact   = Artifact( name )
      Grapheme.Value.Audio( artifact, afSpec, offset = 0L, gain = 1.0 )
   }

   def bounce( chunks: IIdxSeq[ Chunk ])( implicit artifactStore: ArtifactStore ) : Phrase = {
//      if( chunks.isEmpty ) {
//      }

      val sorted  = chunks.sortBy( _.segm.span.start )
      val outF    = GraphemeUtil.createTempFile( suffix = ".aif", dir = None, keep = false )
      val afSpec  = AudioFileSpec( fileType = AudioFileType.AIFF, sampleFormat = SampleFormat.Float,
                                   numChannels = 1, sampleRate = VoiceTrap.sampleRate )
      val afOut   = AudioFile.openWrite( outF, afSpec )
      val bufIn   = afOut.buffer( 8192 )
      val bufInCh = bufIn( 0 )
      val bufOut  = afOut.buffer( 8192 )
      val bufOutCh = bufOut( 0 )
      var current = sorted.head.segm.span.start
      var remain  = sorted
      var active  = IIdxSeq.empty[ Open ]
      var keepGoing  = true

      while( keepGoing ) {
         val (add, defer) = remain.span( _.segm.span.start == current )
         remain   = defer
         active ++= add.map( _.open() )
         val next0   = remain.foldLeft( Long.MaxValue ) { case (res, chunk) => math.min( res, chunk.stop )}
         val next    = active.foldLeft( next0 ) { case (res, open) => math.min( res, open.stop )}
         keepGoing   = next < Long.MaxValue
         if( keepGoing ) {
            while( current < next ) {
               val chunkLen = math.min( next - current, 8192 ).toInt
               DSP.clear( bufOutCh, 0, chunkLen )
               active.foreach { open =>
                  open.af.read( bufIn, 0, chunkLen )
                  open.fadeIn.process(  bufInCh, 0, bufInCh, 0, chunkLen )
                  open.fadeOut.process( bufInCh, 0, bufInCh, 0, chunkLen )
                  DSP.add( bufInCh, 0, bufOutCh, 0, chunkLen )
               }
               afOut.write( bufOut, 0, chunkLen )
               current += chunkLen
            }
            val (remove, keep) = active.span( _.stop == current )
            remove.foreach( _.af.close() )
            active = keep
         }
      }
      afOut.close()
      Phrase.fromFile( outF )
   }

   def intersect( segm: AudioSegment, cover: Span ) : Option[ AudioSegment ] = {
      val oldSpan = segm.span
      cover.intersect( oldSpan ).nonEmptyOption.map { newSpan =>
         val newOffset = segm.value.offset + newSpan.start - oldSpan.start
         segm.copy( span = newSpan, value = segm.value.copy( offset = newOffset ))
      }
   }
}