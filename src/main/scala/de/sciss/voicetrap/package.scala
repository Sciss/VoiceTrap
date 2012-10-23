package de.sciss

import lucre.confluent.reactive.ConfluentReactive
import lucre.{DataOutput, DataInput, stm}
import synth.proc
import proc.Grapheme

package object voicetrap {
   type S               = ConfluentReactive
   type Tx              = S#Tx
   type Acc             = S#Acc
   type ID              = S#ID
   type Var[ A ]        = S#Var[ A ]

   type Source[ A ]     = stm.Source[ Tx, A ]
   type Cursor          = lucre.confluent.Cursor[ S ]
   type ProcGroup       = proc.ProcGroup_.Modifiable[ S ]
   type Serializer[ A ] = stm.Serializer[ Tx, Acc, A ]

   type AudioArtifact   = Grapheme.Value.Audio

   def ??? : Nothing = sys.error( "TODO" )

   def readSerVersion( in: DataInput, expected: Int ) {
      val cookie = in.readUnsignedByte()
      require( cookie == expected, "Expected serialized version " + expected + ", but found " + cookie )
   }

   def writeSerVersion( out: DataOutput, cookie: Int ) {
      out.writeUnsignedByte( cookie )
   }
}