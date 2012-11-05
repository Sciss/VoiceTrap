package de.sciss.voicetrap

import collection.immutable.{IndexedSeq => IIdxSeq }

object BufferVerification extends App {
   val src  = io.Source.fromFile( "notes/osc_dump.txt", "UTF-8" )
   val text = src.getLines().mkString( "\n" )
   src.close()

   def mkPackets( text: String ) : IIdxSeq[ String ] = {
      val res  = IIdxSeq.newBuilder[ String ]
      var i    = text.indexOf( '[', 0 )
      while( i >= 0 ) {
         var cnt  = 1
         var n    = i + 1
         do {
            val j    = text.indexOf( '[', n )
            val k    = text.indexOf( ']', n )
            if( j >= 0 && j < k ) {
               cnt += 1
               n = j + 1
            } else {
               require( k >= 0 )
               cnt -= 1
               n = k + 1
            }
         } while( cnt > 0 )

         res += text.substring( i + 1, n - 1 )
         i    = text.indexOf( '[', n )
      }
      res.result()
   }

   case object Data

   def mkMessages( p: IIdxSeq[ String ]) : IIdxSeq[ IIdxSeq[ Any ]] = {
      p.flatMap { s =>
         val seq = s.trim.split( ", " ).toIndexedSeq
         if( seq.headOption == Some( "#bundle" )) {
//            println( seq.drop( 1 ))
            mkMessages( mkPackets( seq.drop( 1 ).mkString( ", " ))) // .drop( 2 ).mkString( "[ ", ", ", " ]" )))
         } else {
            val msg = seq.map { s2 =>
               if( s2.startsWith( "\"" )) {
                  s2.substring( 1, s2.length - 1 )
               } else if( s2.length > 0 && (s2.charAt( 0 ).isDigit || s2.charAt( 0 ) == '-') ) {
                  val hasDot = s2.indexOf( "." ) >= 0
//                  if( hasDot ) println( "HAS DOT : " + s2 )
                  if( hasDot ) (s2.toDouble): Any else (s2.toInt): Any
               } else if( s2 == "true" ) {
                  true
               } else if( s2 == "false" ) {
                  false
               } else if( s2.startsWith( "DATA" )) {
                  Data
               } else {
                  s2
               }
            }
            IIdxSeq( msg )
         }
      }
   }

   sealed trait BufStatus
   case object BufAlloced extends BufStatus
   case object BufRead    extends BufStatus
   case object BufWritten extends BufStatus
   case class  BufUsed( nodeID: Int ) extends BufStatus
   case object BufUnused extends BufStatus
   case object BufClosed extends BufStatus
   case object BufFree   extends BufStatus

   def verify( m: IIdxSeq[ IIdxSeq[ Any ]]) {
      var bufMap  = Map.empty[ Int, BufStatus ].withDefault( _ => BufFree )
      var nodeMap = Map.empty[ Int, Int ]

      def msgString( msg: Seq[ Any ]) = msg.mkString( "[ ", ", ", " ]" )

      m.foreach {
         case msg @ Seq( "/s_new", _, nodeID: Int, _, _, args @ _* ) =>
            val argIdx = args.indexOf( "buf" )
            if( argIdx >= 0 ) {
               val bIdx = args( argIdx + 1 ).asInstanceOf[ Number ].intValue() // toInt
               bufMap( bIdx ) match {
                  case BufRead =>
                  case BufWritten =>
                  case other => sys.error( msgString( msg ) + " - unexpected: " + other )
               }
               bufMap  += bIdx   -> BufUsed( nodeID )
               nodeMap += nodeID -> bIdx
            }

         case msg @ Seq( "/n_end", nodeID: Int, _* ) =>
            nodeMap.get( nodeID ).foreach { bIdx =>
               nodeMap -= nodeID
               bufMap( bIdx ) match {
                  case BufUsed( `nodeID` ) =>
                  case other => sys.error( msgString( msg ) + " - unexpected: " + other )
               }
               bufMap += bIdx -> BufUnused
            }

         case msg @ Seq( "/b_alloc", bIdx: Int, _* ) =>
            bufMap( bIdx ) match {
               case BufFree =>
               case other => sys.error( msgString( msg ) + " - unexpected: " + other )
            }
            bufMap += bIdx -> BufAlloced

         case msg @ Seq( "/b_write", bIdx: Int, _* ) =>
            bufMap( bIdx ) match {
               case BufAlloced =>
               case other => sys.error( msgString( msg ) + " - unexpected: " + other )
            }
            bufMap += bIdx -> BufWritten

         case msg @ Seq( "/b_read", bIdx: Int, _* ) =>
            bufMap( bIdx ) match {
               case BufAlloced =>
               case other => sys.error( msgString( msg ) + " - unexpected: " + other )
            }
            bufMap += bIdx -> BufRead

         case msg @ Seq( "/b_close", bIdx: Int ) =>
            bufMap( bIdx ) match {
               case BufUnused =>
               case other => sys.error( msgString( msg ) + " - unexpected: " + other )
            }
            bufMap += bIdx -> BufClosed

         case msg @ Seq( "/b_free", bIdx: Int ) =>
            bufMap( bIdx ) match {
               case BufClosed =>
               case other => sys.error( msgString( msg ) + " - unexpected: " + other )
            }
            bufMap += bIdx -> BufFree

         case _ =>
      }
   }

   val p = mkPackets( text )
   // p.foreach( println )

   val m = mkMessages( p )
//   m.foreach(println)

   verify( m )
}