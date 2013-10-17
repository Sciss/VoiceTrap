package de.sciss.lucre.confluent

import de.sciss.voicetrap._

object Analysis extends App {
  run()

  def run(): Unit = {
    val infra = Infra()
    // system.asInstanceOf[de.sciss.synth.proc.Confluent]
    val s = infra.system.asInstanceOf[de.sciss.lucre.confluent.impl.ConfluentImpl.Mixin[S]]
    // readIndexTree( term: Long )( implicit tx: D#Tx )
    infra.cursor.step { implicit tx =>
      // val doc = infra.document.get
      implicit val dtx: s.D#Tx = s.durableTx(tx)
      val acc         = tx.inputAccess
      println(acc)
      val term0       = 16206L // acc.term // 1L << 32
      // val tree  = p(s)('readIndexTree)(term, dtx).asInstanceOf[Sys.IndexTree[s.D]]
      val tree        = s.readIndexTree(term0)
      val (vertx0, _) = s.readTreeVertex(tree.tree, acc.index, term0)
      val term        = vertx0.version
      val (vertx, _)  = s.readTreeVertex(tree.tree, acc.index, term)
      val info        = s.versionInfo(term)
      println(s"tree: $tree")
      println(s"vertex: $vertx; version = ${vertx.versionInt}; info = $info")
    }
    s.close()
  }

  // by jorge ortiz: https://gist.github.com/jorgeortiz85/908035
  class PrivateMethodCaller(x: AnyRef, methodName: String) {
    def apply(_args: Any*): Any = {
      val args = _args.map(_.asInstanceOf[AnyRef])
      def _parents: Stream[Class[_]] = Stream(x.getClass) #::: _parents.map(_.getSuperclass)
      val parents = _parents.takeWhile(_ != null).toList
      val methods = parents.flatMap(_.getDeclaredMethods)
      methods.foreach(m => println(m.getName))
      val method = methods.find(_.getName == methodName).getOrElse(throw new IllegalArgumentException("Method " + methodName + " not found"))
      method.setAccessible(true)
      method.invoke(x, args: _*)
    }
  }

  class PrivateMethodExposer(x: AnyRef) {
    def apply(method: scala.Symbol): PrivateMethodCaller = new PrivateMethodCaller(x, method.name)
  }

  def p(x: AnyRef): PrivateMethodExposer = new PrivateMethodExposer(x)
}
