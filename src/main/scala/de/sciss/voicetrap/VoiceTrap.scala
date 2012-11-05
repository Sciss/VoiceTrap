/*
 *  VoiceTrap.scala
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

import java.io.File
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.AudioBus
import de.sciss.synth.proc.RichGroup
import de.sciss.osc

object VoiceTrap {
   lazy val baseDirectory     : File   = new File( new File( sys.props( "user.home" ), "Desktop" ), "VoiceTrap" )
   lazy val artifactDirectory : File   = new File( baseDirectory, "artifacts" )
//   lazy val databaseDirectory : File   = new File( baseDirectory, "audio_db" )
   lazy val televisionFile    : File   = new File( new File( baseDirectory, "tv" ), "tv.aif" )
   lazy val temporaryDirectory: File   = new File( baseDirectory, "tmp" )

   val minimal                = false
   val liveInput              = true
   val stereoOutput           = false
   val limiterLevel           = 0.9
   val compander              = true
   val hpfFreq                = 50.0

   val jumpBackSound          = true
   val jumpBackSoundVolume    = 1.0
   val shrinkAmount           = 0.15
//   val keepOneProbability     = 0.2

   lazy val numRows           = if( minimal ) /* 2 */ 1 else 4
   lazy val numColumns        = if( minimal ) 1 else 3
   lazy val matrixSize        = numRows * numColumns

   val sampleRate             = 44100.0
   val audioInterface         = "MOTU 828mk2"
   val protocol: osc.Transport.Net = osc.UDP
   val bootServer             = true
   val highestInputChannel    = 2
   val highestOutputChannel   = 22
   val microphoneChannel      = 0
   var microphoneGain         = 1.0 // amp linear
   val outChannels            = (3 to 10) ++ (15 to 18)

   val forkIterations         = if( minimal ) 1 else 6

   val drainProbability       = 0.05 // 0.1

   val recordBufferSize       = 65536 // 32768

//   lazy val internalBusOffset = highestOutputChannel + highestInputChannel

   var privateBus : AudioBus  = null      // XXX TODO: que se puede acer...

   lazy val phraseLength : Motion  = Motion.exprand( 8.0, 24.0 )
   lazy val loopLength : Motion    = if( minimal ) Motion.constant( 45.0 ) else Motion.exprand( 90.0 / 1.1, 90.0 * 1.1 ) // Motion.constant( 90.0 )

   lazy val startTime         = System.currentTimeMillis()

   case class ChannelDB( database: Database, query: DifferanceDatabaseQuery,
                         thinner: DifferanceDatabaseThinner, filler: DifferanceDatabaseFiller )

   lazy val databases = IIdxSeq.tabulate( numRows ) { row => IIdxSeq.tabulate( numColumns ) { col =>
      val mStr       = "_" + (row+1) + "_" + (col+1)
      val dir        = new File( baseDirectory, "audio_db" + mStr )
//      if( !dir.isDirectory ) dir.mkdir()
      val database   = Database( "db" + mStr, dir )
      val query      = DifferanceDatabaseQuery(   database )
      val thinner    = DifferanceDatabaseThinner( database )
      val filler     = DifferanceDatabaseFiller( database, television )
      ChannelDB( database, query, thinner, filler )
   }}

//   lazy val database        : Database                   = Database( databaseDirectory )
//   lazy val databaseQuery   : DifferanceDatabaseQuery    = DifferanceDatabaseQuery(   database )
//   lazy val databaseThinner : DifferanceDatabaseThinner  = DifferanceDatabaseThinner( database )
//   lazy val databaseFiller  : DifferanceDatabaseFiller   = DifferanceDatabaseFiller( database, television )
   lazy val television      : Television                 = if( liveInput ) Television.live() else Television.fromFile( televisionFile )

   var txnThread : Thread = null

   var masterGroup : RichGroup = null

   def main( args: Array[ String ]) {
      Infra().start()
   }
}
