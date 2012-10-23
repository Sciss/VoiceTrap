package de.sciss.voicetrap

//import collection.immutable.{IndexedSeq => IIdxSeq}
import impl.{DocumentImpl => Impl}
import de.sciss.lucre.Writable
import de.sciss.synth.proc.AuralSystem

object Document {
   implicit def serializer: Serializer[ Document ] = Impl.serializer

   def apply()( implicit tx: Tx ) : Document = {
      log( "New document" )
      Impl()
   }
}
trait Document extends Writable {
   def cursor: Cursor

   def artifactStore( implicit tx: Tx ) : ArtifactStore

//   /**
//    * Map from (row, column) to channel
//    */
//   def channels : Map[ (Int, Int), Channel ] // Var[ Map[ (Int, Int), Channel ]]

   /**
    * Fork random range bounds in seconds
    */
   def minMaxFork : (Double, Double) // Var[ (Double, Double) ]

//   /**
//    * One group is shared across channels, which merely
//    * differ in their cursor access...
//    */
//   def group( implicit tx: Tx ) : ProcGroup // Source[ ProcGroup ]

   /**
    * Wrapping duration in seconds for the performance time
    */
   def pDur : Double // Var[ Double ]

   def start( auralSystem: AuralSystem[ S ])( implicit tx: Tx ) : Unit
   def stop()( implicit tx: Tx ) : Unit
}