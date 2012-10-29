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
   def baseDirectory     : File  = new File( new File( sys.props( "user.home" ), "Desktop" ), "VoiceTrap" )
   def artifactDirectory : File  = new File( baseDirectory, "artifacts" )
   def databaseDirectory : File  = new File( baseDirectory, "audio_db" )

   val minimal                = true

   def numRows                = if( minimal ) 2 else 4
   def numColumns             = if( minimal ) 1 else 3
   def matrixSize             = numRows * numColumns

   def sampleRate             = 44100.0
   def audioInterface         = "MOTU 828mk2"
   def highestInputChannel    = 2
   def highestOutputChannel   = 22

   def phraseLength : Motion  = Motion.exprand( 8.0, 24.0 )
   def loopLength : Motion    = Motion.constant( 90.0 )

   lazy val database        : Database                   = Database( databaseDirectory )
   lazy val databaseQuery   : DifferanceDatabaseQuery    = DifferanceDatabaseQuery(   database )
   lazy val databaseThinner : DifferanceDatabaseThinner  = DifferanceDatabaseThinner( database )

   def main( args: Array[ String ]) {
      Infra().start()
   }
}