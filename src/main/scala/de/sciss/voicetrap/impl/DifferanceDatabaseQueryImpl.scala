/*
 *  DifferanceDatabaseQueryImpl.scala
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
package impl

import de.sciss.strugatzki.{FeatureCorrelation, Span => SSpan}
import java.io.File
import de.sciss.synth
import concurrent.stm.InTxn
import de.sciss.lucre.bitemp.Span

object DifferanceDatabaseQueryImpl {
   var verbose = false

   def apply( db: Database ) : DifferanceDatabaseQuery = new DifferanceDatabaseQueryImpl( db )
}
class DifferanceDatabaseQueryImpl private ( db: Database ) extends AbstractDifferanceDatabaseQuery {
   import GraphemeUtil._
   import DifferanceDatabaseQuery._

   private val identifier  = "database-query-impl"

   val matchDurationMotion    = Motion.coin(
      1.0/33,
      Motion.linexp( Motion.walk( 0, 1, 0.1 ), 0, 1, 0.4, 4.0 ), // Motion.exprand( 0.4, 4.0 )
      Motion.exprand( 4.0, 16.0 )
   )
//   val matchDeviationMotion   = Motion.linrand( 0.2, 0.5 )
   val spectralMotion         = Motion.linrand( 0.25, 0.75 )
//   val stretchDeviationMotion = Motion.walk( 0.111, 0.333, 0.05 ) // Motion.linrand( 0.2, 0.5 )
//   val rankMotion             = Motion.linrand( 0, 11 )

   val maxBoostMotion         = Motion.constant( 30 ) // 18
   val minSpacingMotion       = Motion.constant( 0.0 ) // 0.5

//   val minPhraseDur           = 10.0

   def findMatch( rank: Int, phrase: Phrase, weight: Double )( implicit tx: InTxn ) : FutureResult[ Match ] = {

      import synth._

      val maxBoost   = maxBoostMotion.step().dbamp.toFloat
      val minSpc     = secondsToFrames( minSpacingMotion.step() )
      val dirFut     = db.asStrugatziDatabase
      val metaFut    = phrase.asStrugatzkiInput

      dirFut.flatMapSuccess { dir =>
         metaFut.flatMapSuccess { metaInput =>
            findMatchIn( dir, metaInput, maxBoost, minSpc, rank, phrase, weight )
         }
      }
   }

   private def findMatchIn( dir: File, metaInput: File, maxBoost: Float, minSpacing: Long, rank: Int, phrase: Phrase,
                            weight: Double ) : FutureResult[ Match ] = {

      import FeatureCorrelation.{Match => _, _} // otherwise Match shadows DifferanceDatabaseQuery.Match

      val res              = FutureResult.event[ Match ]()
      val set              = SettingsBuilder()
      set.databaseFolder   = dir
      set.normalize        = true
      set.maxBoost         = maxBoost
      set.metaInput        = metaInput
      set.minSpacing       = minSpacing
      set.numMatches       = max( 2, rank + 1 )
      set.numPerFile       = max( 2, rank + 1 )

      val punchIn          = Span( 0L, phrase.length )

      set.punchIn          = Punch( SSpan( punchIn.start, punchIn.stop ), weight.toFloat )
      set.punchOut         = None // Some( Punch( SSpan( punchOut.start, punchOut.stop ), weight.toFloat ))
//      set.minPunch         = minPunch
//      set.maxPunch         = maxPunch

      val setb             = set.build

      if( verbose ) println( "----CORRELATION----" )
      if( verbose ) println( setb )

      val process          = apply( setb ) {
         case Aborted =>
            val e = new RuntimeException( identifier + " process aborted" )
            res.fail( e )

         case Failure( e ) =>
            res.fail( e )

         case Success( coll ) =>
            val idx0 = min( coll.size - 1, rank )
            val idx  = if( idx0 == 0 && coll( idx0 ).sim.isNaN ) idx0 + 1 else idx0
            if( idx < 0 || idx >= coll.size ) {
               val e = new RuntimeException( identifier + " process yielded no matches" )
               res.fail( e )
            } else {
               val m = coll( idx )
               res.succeed( Match( db, Span( m.punch.start, m.punch.stop ), m.boostIn, m.boostOut ))
            }

         case Progress( p ) =>
      }
      process.start()
      res
   }

//   private def failureMatch = {
//      atomic( identifier + " : failure match" ) { tx1 =>
//         Match( db, Span( 0L, min( db.length( tx1 ), secondsToFrames( 1.0 ))), 1f, 1f )
//      }
//   }
}