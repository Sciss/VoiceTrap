package de.sciss.voicetrap

import java.io.File

object VoiceTrap {
   def baseDirectory : File   = new File( new File( sys.props( "user.home" ), "Desktop" ), "VoiceTrap" )

   val minimal                = true

   def numRows                = if( minimal ) 1 else 4
   def numColumns             = if( minimal ) 2 else 3
   def matrixSize             = numRows * numColumns

   def sampleRate             = 44100.0
   def audioInterface         = "MOTU 828mk2"
   def highestInputChannel    = 2
   def highestOutputChannel   = 22

   def main( args: Array[ String ]) {
      Infra().start()
   }
}