package de.sciss.voicetrap

//import collection.immutable.{IndexedSeq => IIdxSeq}

object Document {
   implicit def serializer: Serializer[ Document ] = ???
}
trait Document {
   /**
    * Map from (row, column) to channel
    */
   def channels : Map[ (Int, Int), Channel ] // Var[ Map[ (Int, Int), Channel ]]

   /**
    * Fork random range bounds in seconds
    */
   def minMaxFork : (Double, Double) // Var[ (Double, Double) ]

   /**
    * One group is shared across channels, which merely
    * differ in their cursor access...
    */
   def group : ProcGroup // Source[ ProcGroup ]

   /**
    * Wrapping duration in seconds for the performance time
    */
   def pDur : Double // Var[ Double ]
}