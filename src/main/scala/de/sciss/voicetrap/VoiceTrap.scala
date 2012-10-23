package de.sciss.voicetrap

import java.io.File

object VoiceTrap {
   def baseDirectory : File = new File( new File( sys.props( "user.home" ), "Desktop" ), "VoiceTrap" )

   def numRows    = 4
   def numColumns = 3
}