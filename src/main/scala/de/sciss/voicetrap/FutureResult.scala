/*
 *  FutureResult.scala
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

import actors.sciss.FutureActor
import actors.{Futures, Future}
import concurrent.stm.Txn

trait FutureResult[ +A ] {
   import FutureResult._

   def isSet : Boolean
   def apply() : Result[ A ]
   def map[ B ]( fun: Result[ A ] => Result[ B ]) : FutureResult[ B ]
   def flatMap[ B ]( fun: Result[ A ] => FutureResult[ B ]) : FutureResult[ B ]

   final def mapSuccess[ B ]( fun: A => Result[ B ]) : FutureResult[ B ] =
      map {
         case Success( value ) => fun( value )
         case f @ Failure( _ ) => f
      }

   final def flatMapSuccess[ B ]( fun: A => FutureResult[ B ]) : FutureResult[ B ] =
      flatMap {
         case Success( value ) => fun( value )
         case f @ Failure( _ ) => now( f )
      }

   /* private[grapheme] */ def peer : Future[ Result[ A ]]
}

object FutureResult {
   object Result {
      implicit def succeed[ A ]( value: A ) : Result[ A ] = Success( value )
   }

   sealed trait Result[ +A ] {
      def toOption: Option[ A ]

//      /**
//       * Returns the result value in case of success,
//       * or rethrows the exception in case of failure
//       */
//      def get : A
   }
   final case class Failure( e: Throwable ) extends Result[ Nothing ] {
      def toOption = None
//      def get = throw e
   }
   final case class Success[ A ]( value: A ) extends Result[ A ] {
      def toOption = Some( value )
//      def get = value
   }

   def now[ A ]( value: Result[ A ]) : FutureResult[ A ] = {
      val ev = event[ A ]()  // hmmmm... too much effort?
      ev.set( value )
      ev
   }

   def nowSucceed[ A ]( value: A ) : FutureResult[ A ] = now( Success( value ))
   def nowFail[ A ]( e: Throwable ) : FutureResult[ A ] = now( Failure( e ))

   trait Event[ A ] extends FutureResult[ A ] {
      def set( result: Result[ A ]) : Unit
      final def succeed( value :A ) { set( Success( value ))}
      final def fail( e: Throwable ) { set( Failure( e ))}
   }

   def event[ A ]() : FutureResult.Event[ A ] = new FutureResult.Event[ A ] with Basic[ A ] {
      case class Set( value: Result[ A ]) // warning: don't make this final -- scalac bug

      val c = FutureActor.newChannel[ Result[ A ]]()
      val peer: FutureActor[ Result[ A ]] = new FutureActor[ Result[ A ]]({ syncVar =>
         peer.react {
            case Set( value ) => syncVar.set( value )
         }
      }, c )
      peer.start()

      def set( value: Result[ A ]) { peer ! Set( value )}
   }

   private def wrap[ A ]( fut: Future[ Result[ A ]]) : FutureResult[ A ] =
      new FutureResult[ A ] with Basic[ A ] {
         def peer = fut
      }

   private sealed trait Basic[ A ] {
      me: FutureResult[ A ] =>

      def map[ B ]( fun: Result[ A ] => Result[ B ]) : FutureResult[ B ] = wrap( Futures.future {
         try {
            fun( me.peer.apply() )
         } catch {
            case e: Throwable => Failure( e )
         }
      })

      def flatMap[ B ]( fun: Result[ A ] => FutureResult[ B ]) : FutureResult[ B ] = wrap( Futures.future {
         try {
            fun( me.peer.apply() ).peer.apply()
         } catch {
            case e: Throwable => Failure( e )
         }
      })

      def isSet : Boolean = peer.isSet

      def apply() : Result[ A ] = {
         require( Txn.findCurrent.isEmpty, "Must not call future-apply within an active transaction" )
         peer.apply()
      }
   }
}