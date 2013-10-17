package de.sciss.lucre.confluent

import de.sciss.voicetrap._
import de.sciss.play.json.{Formats, AutoFormat}
import de.sciss.file._
import collection.immutable.{IndexedSeq => Vec}
import language.implicitConversions
import play.api.libs.json.Format

object Analysis extends App {
  run()

  // from   6 Nov 2012, 16:21'23.704  (version 1, weil version 0 keinen korrekten stamp liefert)
  // until  6 Nov 2012, 19:01'31.943
  def lastDBVersion       = 16206   // as left behind after december 2012
  def lastInstallVersion  = 15703   // as run up to 06-Nov-12 19:01'31.943

  case class Version(id: Int, rand: Int, stamp: Long, parent: Int) {
    def term: Long = (rand.toLong << 32) | (id.toLong & 0xFFFFFFFFL)
  }

  def jsonFile = userHome / "Desktop" / "voicetrap_versions.json"

  implicit lazy val versionFormat : Format[Version]       = AutoFormat[Version]
  implicit lazy val versionsFormat: Format[Vec[Version]]  = Formats.VecFormat[Version]

  def run(): Unit = {
    generateJSON()

  }

  def generateJSON(): Unit = {
    if (jsonFile.isFile) {
      println(s"File '$jsonFile' already exists.")
      return
    }

    val infra = Infra()
    // system.asInstanceOf[de.sciss.synth.proc.Confluent]
    val s = infra.system.asInstanceOf[de.sciss.lucre.confluent.impl.ConfluentImpl.Mixin[S]]
    try {
      // readIndexTree( term: Long )( implicit tx: D#Tx )
      val versions = infra.cursor.step { implicit tx =>
        // val doc = infra.document.get
        implicit val dtx: s.D#Tx = s.durableTx(tx)
        val acc         = tx.inputAccess
        val index       = acc.index   // always <v0> because we didn't use melding
        // println(acc)

        var res = List.empty[Version]

        for (v0  <- 1 to lastInstallVersion) {
          // val term0       = 1 // lastInstallVersion  // (maxVersion - 1).toLong // acc.term // 1L << 32
          // val tree  = p(s)('readIndexTree)(term, dtx).asInstanceOf[Sys.IndexTree[s.D]]
          val term0       = v0.toLong // leave randomised part empty
          val tree        = s.readIndexTree(term0)
          val (vertx0, _) = s.readTreeVertex(tree.tree, acc.index, term0)
          val term        = vertx0.version
          val (vertx, _)  = s.readTreeVertex(tree.tree, acc.index, term)
          val info        = s.versionInfo(term)
          // info.timeStamp
          // s.isAncestor(index, term1, term2)
          // println(s"tree: $tree")
          // println(s"vertex: $vertx; version = ${vertx.versionInt}; info = $info")

          val parentID = res.find(p => s.isAncestor(index, p.term, term)).map(_.id).getOrElse(-1)
          res = Version(id = vertx.versionInt, rand = (vertx.version >> 32).toInt, stamp = info.timeStamp,
            parent = parentID) :: res

          if (v0 % 100 == 0) println(s"v$v0")
        }
        res.reverse.toIndexedSeq
      }

      println(s"Retrieved ${versions.size} versions. Writing to '$jsonFile'")
      JsIO.write(versions, jsonFile)

    } finally {
      s.close()
    }
  }

  //  // by jorge ortiz: https://gist.github.com/jorgeortiz85/908035
  //  class PrivateMethodCaller(x: AnyRef, methodName: String) {
  //    def apply(_args: Any*): Any = {
  //      val args = _args.map(_.asInstanceOf[AnyRef])
  //      def _parents: Stream[Class[_]] = Stream(x.getClass) #::: _parents.map(_.getSuperclass)
  //      val parents = _parents.takeWhile(_ != null).toList
  //      val methods = parents.flatMap(_.getDeclaredMethods)
  //      methods.foreach(m => println(m.getName))
  //      val method = methods.find(_.getName == methodName).getOrElse(throw new IllegalArgumentException("Method " + methodName + " not found"))
  //      method.setAccessible(true)
  //      method.invoke(x, args: _*)
  //    }
  //  }
  //
  //  class PrivateMethodExposer(x: AnyRef) {
  //    def apply(method: scala.Symbol): PrivateMethodCaller = new PrivateMethodCaller(x, method.name)
  //  }
  //
  //  def p(x: AnyRef): PrivateMethodExposer = new PrivateMethodExposer(x)
}
