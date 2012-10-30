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

object VoiceTrap {
   lazy val baseDirectory     : File   = new File( new File( sys.props( "user.home" ), "Desktop" ), "VoiceTrap" )
   lazy val artifactDirectory : File   = new File( baseDirectory, "artifacts" )
   lazy val databaseDirectory : File   = new File( baseDirectory, "audio_db" )
   lazy val televisionFile    : File   = new File( new File( baseDirectory, "tv" ), "tv.aif" )
   lazy val temporaryDirectory: File   = new File( baseDirectory, "tmp" )

   val minimal                = true
   val liveInput              = false

   lazy val numRows           = if( minimal ) 2 else 4
   lazy val numColumns        = if( minimal ) 1 else 3
   lazy val matrixSize        = numRows * numColumns

   val sampleRate             = 44100.0
   val audioInterface         = "MOTU 828mk2"
   val highestInputChannel    = 2
   val highestOutputChannel   = 22

   lazy val phraseLength : Motion  = Motion.exprand( 8.0, 24.0 )
   lazy val loopLength : Motion    = Motion.constant( 90.0 )

   lazy val database        : Database                   = Database( databaseDirectory )
   lazy val databaseQuery   : DifferanceDatabaseQuery    = DifferanceDatabaseQuery(   database )
   lazy val databaseThinner : DifferanceDatabaseThinner  = DifferanceDatabaseThinner( database )
   lazy val television      : Television                 = if( liveInput ) Television.live() else Television.fromFile( televisionFile )
   lazy val databaseFiller  : DifferanceDatabaseFiller   = DifferanceDatabaseFiller( database, television )

   def main( args: Array[ String ]) {
      Infra().start()
   }
}