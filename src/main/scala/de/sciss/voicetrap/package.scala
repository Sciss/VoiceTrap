package de.sciss

import lucre.confluent.reactive.ConfluentReactive
import lucre.stm
import synth.proc

package object voicetrap {
   type S            = ConfluentReactive
   type Tx           = S#Tx
   type Acc          = S#Acc
   type ID           = S#ID
   type Var[ A ]     = S#Var[ A ]

   type Source[ A ]  = stm.Source[ S#Tx, A ]
   type Cursor       = lucre.confluent.Cursor[ S ]
   type ProcGroup    = proc.ProcGroup[ S ]

   def ??? : Nothing = sys.error( "TODO" )
}
