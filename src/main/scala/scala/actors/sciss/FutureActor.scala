package scala.actors
package sciss

import concurrent.SyncVar
import scheduler.DaemonScheduler

/**
* `scala.actors` unfortunately is largely misdesigned. One problem with `Futures` is that
* `FutureActor` is private, not extensible, and on top of that, `Future` has package private
* fields that cannot be accessed, unless we hijack package `scala.actors`.
*/
object FutureActor {
   def newChannel[ A ]() : Channel[ A ] = new Channel[ A ]( Actor.self( DaemonScheduler ))
}
class FutureActor[T](fun: SyncVar[T] => Unit, channel: Channel[T]) extends Future[T] with DaemonActor {

  var enableChannel = false // guarded by this

  def isSet = !fvalue.isEmpty

  def apply(): T = {
    if (fvalue.isEmpty) {
      this !? Eval
    }
    fvalueTyped
  }

  def respond(k: T => Unit) {
    if (isSet) k(fvalueTyped)
    else {
      val ft = this !! Eval
      ft.inputChannel.react {
        case _ => k(fvalueTyped)
      }
    }
  }

  def inputChannel: InputChannel[T] = {
    synchronized {
      if (!enableChannel) {
        if (isSet)
          channel ! fvalueTyped
        enableChannel = true
      }
    }
    channel
  }

  def act() {
    val res = new SyncVar[T]

    {
      fun(res)
    } andThen {

      synchronized {
        val v = res.get
        fvalue =  Some(v)
        if (enableChannel)
          channel ! v
      }

      loop {
        react {
          case Eval => reply()
        }
      }
    }
  }
}
