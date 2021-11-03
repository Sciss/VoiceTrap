package de.sciss.voicetrap
package impl

object DummyLimiterImpl extends SignalLimiter {
  def apply(lookAhead: Int, ceil: Float = 0.977f): SignalLimiter = this

  def latency = 0

  def process(in: Array[Float], inOff: Int, out: Array[Float], outOff: Int, len: Int): Int = {
    System.arraycopy(in, inOff, out, outOff, len)
    len
  }
}