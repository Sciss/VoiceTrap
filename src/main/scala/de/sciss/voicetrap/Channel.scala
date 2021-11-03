package de.sciss.voicetrap

import de.sciss.lucre.Writable
import de.sciss.synth.proc
import de.sciss.synth.proc.Server
import de.sciss.voicetrap.impl.{ChannelImpl => Impl}

object Channel {
  implicit def serializer: Serializer[Channel] = Impl.serializer

  def apply(row: Int, column: Int, group: ProcGroup)(implicit tx: Tx): Channel = Impl(row, column, group)
}

trait Channel extends Writable {
  def row: Int

  def column: Int

  def hiddenLayer: AudioArtifact
  //   def cursor( implicit tx: Tx ): Cursor

  //   def refresh( implicit tx: Tx ) : Channel

  def insert(segm: AudioSegment)(implicit tx: Tx): Unit

  def removeAt(time: Long)(implicit tx: Tx): Unit

  def removeFrom(time: Long)(implicit tx: Tx): Unit

  def nextSearch(loop: Long, iter: Int, iterZeroTime: Long, document: Document, auralSystem: proc.AuralSystem[S],
                 server: Server, transportOption: Option[Transport])(implicit tx: Tx, cursor: Cursor): Unit

  // ---- algorithm ----

  //   def start( document: Document, auralSystem: AuralSystem[ S ])( implicit tx: Tx ) : Unit
  def start(document: Document, server: Server, auralSystem: proc.AuralSystem[S])(implicit tx: Tx, cursor: Cursor): Unit

  def stop()(implicit tx: Tx): Unit

  //   def fork()( implicit tx: Tx ) : Unit
}