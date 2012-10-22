package de.sciss.voicetrap

object Infra {

}
trait Infra {
   def system: S
   def document: Source[ Document ]
   def start() : Unit
}