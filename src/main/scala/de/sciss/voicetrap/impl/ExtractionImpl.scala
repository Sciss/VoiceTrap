/*
 *  ExtractionImpl.scala
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

import java.io.File
import de.sciss.strugatzki.FeatureExtraction

trait ExtractionImpl {
   import GraphemeUtil._

   protected def identifier: String

   /**
    * Starts an extraction process for a given audio file input. The process
    * is started after the transaction commits, and the method returns
    * the future result of the meta file thus generated.
    */
   protected def extract( audioInput : File, dir: Option[ File ], keep: Boolean ) /* ( implicit tx: Tx ) */ : FutureResult[ File ] = {
      val res = FutureResult.event[ File ]( "extract")
//      Txn.afterCommit { _ =>
         import FeatureExtraction._
         val set           = SettingsBuilder()
         set.audioInput    = audioInput
         set.featureOutput = createTempFile( ".aif", dir, keep )
         val meta          = createTempFile( "_feat.xml", dir, keep )
         set.metaOutput    = Some( meta )
         val process       = apply( set ) {
            case Aborted =>
               val e = new RuntimeException( identifier + " process aborted" )
               res.fail( e )

            case Failure( e ) =>
               res.fail( e )

            case Success( _ ) =>
               var keepWaiting = 10
               while( keepWaiting > 0 ) {
                  if( meta.isFile && meta.length() > 0L ) keepWaiting = 0
                  if( keepWaiting > 0 ) {
                     keepWaiting -= 1
                     Thread.sleep( 100 )
                  }
               }
               res.succeed( meta )

            case Progress( p ) =>
         }
         process.start()
//      }
      res
   }
}