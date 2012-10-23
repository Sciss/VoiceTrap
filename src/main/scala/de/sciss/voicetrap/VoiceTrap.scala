package de.sciss.voicetrap

import java.io.File

object VoiceTrap {
   def baseDirectory : File   = new File( new File( sys.props( "user.home" ), "Desktop" ), "VoiceTrap" )

   def numRows                = 4
   def numColumns             = 3
   def matrixSize             = numRows * numColumns

   def sampleRate             = 44100.0
   def audioInterface         = "MOTU 828mk2"
   def highestInputChannel    = 2
   def highestOutputChannel   = 22
}