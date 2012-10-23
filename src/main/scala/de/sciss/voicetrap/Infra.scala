package de.sciss.voicetrap

import impl.{InfraImpl => Impl}

object Infra {
   private lazy val instance = Impl()

   def apply() : Infra = instance
}
trait Infra {
   def system: S
   def document: Source[ Document ]
   def start() : Unit
}