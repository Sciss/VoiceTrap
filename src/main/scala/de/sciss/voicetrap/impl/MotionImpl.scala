/*
 *  MotionImpl.scala
 *  (VoiceTrap)
 *
 *  Copyright (c) 2012-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.voicetrap
package impl

import de.sciss.synth

import scala.concurrent.stm.{InTxn, Ref}

object MotionImpl {

  import GraphemeUtil._

  type Tx = InTxn

  //   private val seed  = System.currentTimeMillis()  // 0L
  //   private val rng   = new util.Random( seed )
  //   private def random( implicit tx: Tx ) : Double = rng.nextDouble()
  //   private def max( i: Int, is: Int* ) : Int = is.foldLeft( i )( _ max _ )
  //   private def max( n: Long, ns: Long* ) : Long = ns.foldLeft( n )( _ max _ )
  //   private def max( d: Double, ds: Double* ) : Double = ds.foldLeft( d )( _ max _ )
  //   private def min( i: Int, is: Int* ) : Int = is.foldLeft( i )( _ min _ )
  //   private def min( n: Long, ns: Long* ) : Long = ns.foldLeft( n )( _ min _ )
  //   private def min( d: Double, ds: Double* ) : Double = ds.foldLeft( d )( _ min _ )

  def constant(value: Double): Motion = Constant(value)

  def linrand(lo: Double, hi: Double): Motion = LinRand(lo, hi)

  def exprand(lo: Double, hi: Double): Motion = ExpRand(lo, hi)

  def sine(lo: Double, hi: Double, period: Int): Motion = Sine(lo, hi, period)

  def walk(lo: Double, hi: Double, maxStep: Double): Motion = Walk(lo, hi, maxStep)

  def linlin(in: Motion, inLo: Double, inHi: Double, outLo: Double, outHi: Double): Motion =
    LinLin(in, inLo, inHi, outLo, outHi)

  def linexp(in: Motion, inLo: Double, inHi: Double, outLo: Double, outHi: Double): Motion =
    LinExp(in, inLo, inHi, outLo, outHi)

  def coin(prob: Double, a: Motion, b: Motion): Motion = Coin(prob, a, b)

  private final case class Constant(value: Double) extends Motion {
    def step()(implicit tx: Tx): Double = value
  }

  private final case class LinRand(lo: Double, hi: Double) extends Motion {
    val range: Double = hi - lo

    def step()(implicit tx: Tx): Double = random * range + lo
  }

  private final case class ExpRand(lo: Double, hi: Double) extends Motion {
    val factor: Double = math.log(hi / lo)

    def step()(implicit tx: Tx): Double = math.exp(random * factor) * lo
  }

  private final case class Walk(lo: Double, hi: Double, maxStep: Double) extends Motion {
    val maxStep2: Double = maxStep * 2
    val current: Ref[Double] = Ref(Double.NaN)

    def step()(implicit tx: Tx): Double = {
      val c = current()
      val v = if (c.isNaN) {
        random * (hi - lo) + lo
      } else {
        max(lo, min(hi, c + (random * maxStep2 - maxStep)))
      }
      current.set(v)
      v
    }
  }

  private final case class Sine(lo: Double, hi: Double, period: Int) extends Motion {
    val phase: Ref[Int] = Ref(0)
    val mul: Double = (hi - lo) / 2
    val add: Double = mul + lo
    val factor: Double = math.Pi * 2 / period

    def step()(implicit tx: Tx): Double = {
      val p = phase()
      phase.set((p + 1) % period)
      math.sin(p * factor) * mul + add
    }
  }

  private final case class Coin(prob: Double, a: Motion, b: Motion)
    extends Motion {
    def step()(implicit tx: Tx): Double = {
      val m = if (random >= prob) a else b
      m.step()
    }
  }

  private final case class LinLin(in: Motion, inLo: Double, inHi: Double, outLo: Double, outHi: Double)
    extends Motion {
    def step()(implicit tx: Tx): Double = {
      import synth._
      in.step.linlin(inLo, inHi, outLo, outHi)
    }
  }

  private final case class LinExp(in: Motion, inLo: Double, inHi: Double, outLo: Double, outHi: Double)
    extends Motion {
    def step()(implicit tx: Tx): Double = {
      import synth._
      in.step.linexp(inLo, inHi, outLo, outHi)
    }
  }
}