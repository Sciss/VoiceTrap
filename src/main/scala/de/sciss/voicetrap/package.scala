package de.sciss

import lucre.confluent.reactive.ConfluentReactive
import lucre.{DataOutput, DataInput, stm}
import synth.expr.ExprImplicits
import synth.proc
import proc.Grapheme
import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService}

package object voicetrap {
   type S               = ConfluentReactive

   object implicits extends ExprImplicits[ S ]

   type D               = stm.Durable
   type I               = stm.InMemory
   type Tx              = S#Tx
   type Acc             = S#Acc
   type ID              = S#ID
   type Var[ A ]        = S#Var[ A ]

   type Source[ A ]     = stm.Source[ Tx, A ]
   type Cursor          = lucre.confluent.Cursor[ S ]
   type ProcGroup       = proc.ProcGroup_.Modifiable[ S ]
   type Serializer[ A ] = stm.Serializer[ Tx, Acc, A ]

   type AudioArtifact   = Grapheme.Value.Audio
   type Transport       = proc.ProcTransport[ S ]
   type ArtifactStore   = proc.ArtifactStore[ S ]

   def ??? : Nothing = sys.error( "TODO" )

   def readSerVersion( in: DataInput, expected: Int ) {
      val cookie = in.readUnsignedByte()
      require( cookie == expected, "Expected serialized version " + expected + ", but found " + cookie )
   }

   def writeSerVersion( out: DataOutput, cookie: Int ) {
      out.writeUnsignedByte( cookie )
   }

   def mapSerializer[ A, B ]( implicit entrySerializer: Serializer[ (A, B) ]) = stm.Serializer.map[ Tx, Acc, A, B ]

   implicit def artifactStoreSerializer   = proc.ArtifactStore.serializer[ S ]
   implicit def procGroupSerializer       = proc.ProcGroup_.Modifiable.serializer[ S ]

   private lazy val pool : ExecutorService = {        // system wide scheduler
      val res = Executors.newSingleThreadExecutor()
      sys.addShutdownHook( shutdownThreadPool() )
      res
   }

   private def shutdownThreadPool() {
     pool.shutdown()
   }

   def spawn( cursor: Cursor )( fun: Tx => Unit ) {
      pool.submit( new Runnable {
         def run() {
            cursor.step( fun )
         }
      })
   }
}