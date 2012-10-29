package de.sciss.voicetrap

import java.io.File

object VoiceTrap {
   def baseDirectory : File      = new File( new File( sys.props( "user.home" ), "Desktop" ), "VoiceTrap" )
   def artifactDirectory : File  = new File( baseDirectory, "artifacts" )
   def databaseDirectory : File  = new File( baseDirectory, "database" )

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

   def main( args: Array[ String ]) {
      Infra().start()
   }
}