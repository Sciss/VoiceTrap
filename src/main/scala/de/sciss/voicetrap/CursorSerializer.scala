package de.sciss.voicetrap

import de.sciss.lucre.{DataInput, DataOutput, stm}

object CursorSerializer extends stm.Serializer[ Tx, Acc, Cursor ] {
   def write( v: Cursor, out: DataOutput ) { v.write( out )}
   def read( in: DataInput, access: Acc )( implicit tx: Tx ) : Cursor = {
      tx.readCursor( in, access )
   }
}
