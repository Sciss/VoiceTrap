package de.sciss.lucre.confluent

import de.sciss.voicetrap._
import de.sciss.play.json.{Formats, AutoFormat}
import collection.immutable.{IndexedSeq => Vec}
import language.implicitConversions
import play.api.libs.json.Format
import java.io.PrintStream
import de.sciss.file._
import de.sciss.pdflitz
import scala.swing.Swing
import Swing._
import de.sciss.pdflitz.Generate.QuickDraw
import java.awt.{Shape, Color}
import java.awt.geom.{Line2D, Point2D, Ellipse2D}

object Analysis extends App {
  run()

  // from   6 Nov 2012, 16:21'23.704  (version 1, weil version 0 keinen korrekten stamp liefert)
  // until  6 Nov 2012, 19:01'31.943
  def lastDBVersion       = 16206   // as left behind after december 2012
  def lastInstallVersion  = 15703   // as run up to 06-Nov-12 19:01'31.943

  def firstStamp  = 1352215283704L
  def lastStamp   = 1352224891943L
  // 1352217362394L

  case class Version(id: Int, rand: Int, stamp: Long, parent: Int) {
    def term: Long = (rand.toLong << 32) | (id.toLong & 0xFFFFFFFFL)
  }

  def trunc = true

  def jsonFile  = userHome / "Desktop" / (if (trunc) "trunc.json" else "voicetrap_versions.json")
  def dotFile   = jsonFile.replaceExt("dot")
  def pdfFile   = jsonFile.replaceExt("pdf")

  implicit lazy val versionFormat : Format[Version]       = AutoFormat[Version]
  implicit lazy val versionsFormat: Format[Vec[Version]]  = Formats.VecFormat[Version]

  def run(): Unit = {
    generateJSON()
    // generateDOT()
    generatePDF()
  }

  case class Node(id: Int)(x0: Double, val y: Double, val pinned: Boolean) {
    // var children  = Vec.empty[Node]
    var x         = x0

    def shape(radius: Double): Shape = new Ellipse2D.Double(x - radius, y - radius, radius*2, radius*2)
    def location: Point2D = new Point2D.Double(x, y)
  }

  def generatePDF(overwrite: Boolean = true): Unit = {
    if (!overwrite && pdfFile.isFile) {
      println(s"File '$pdfFile' already exists.")
      return
    }
    val versions  = JsIO.read[Vec[Version]](jsonFile).get
    var eliminate = 1     // don't include the main cursor
    var xpos      = 0.0

    var nodeMap   = Map.empty[Int, Node]
    var edges     = Set.empty[(Node, Node)]

    val yLen      = 10.0
    val xSpc      = 30.0
    val yLenSq    = yLen * yLen

    versions.foreach { v =>
      if (v.id == eliminate || v.parent == eliminate) {
        eliminate = v.id
      } else {
        val n = if (v.parent == -1) {
          val x = xpos
          xpos += xSpc
          Node(v.id)(x0 = x, y = 0.0, pinned = true)
        } else {
          // println(s"  ${v.parent} -> ${v.id};")
          val p = nodeMap(v.parent)
          val c = Node(v.id)(x0 = p.x, y = p.y + yLen, pinned = false)
          edges += p -> c
          c
        }
        nodeMap += v.id -> n
      }
    }

    val nodes   = nodeMap.values.toIndexedSeq
    val numIter = 100
    val dt      = 0.05  // delta time in integration
    val minRank = 10    // ignore vertices whose vertical spacing is too large

    // ---- layout ----
    // attraction and repulsion following Chernobelskiy et al. 2011
    for (iter <- 1 to numIter) {
      println(s"Layout iteration $iter")
      // var forces = Map.empty[Node, Double] withDefaultValue 0.0
      val forces = nodes.map { n =>
        if (n.pinned) 0.0 else nodes.view.map { n1 =>
          if (math.abs(n1.y - n.y) < minRank && n != n1) {
            val isCon = edges.contains(n -> n1) || edges.contains(n1 -> n)
            val dx    = n1.x - n.x
            val dy    = n1.y - n.y
            val dist  = math.sqrt(dx * dx + dy * dy)
            val ang   = if (dx != 0) math.atan2(dy, dx) else {
              if (n.id < n1.id) math.Pi else 0.0
            }
            val cos   = math.cos(ang)
            if (isCon) {
              val f = (dist - yLen) / dist
              cos * f
            } else {
              val f = yLenSq / math.max(1.0, dist * dist * dist)
              -cos * f
            }

          } else {
           0.0
          }
        } .sum * dt
      }
      (nodes zip forces).foreach { case (n, f) => n.x += f }
    }

    val (left, top, right, bottom) = ((0.0, 0.0, 0.0, 0.0) /: nodes) {
      case ((x0, y0, x1, y1), n) => (math.min(x0, n.x), math.min(y0, n.y), math.max(x1, n.x), math.max(y1, n.y))
    }
    val margin  = 5.0
    val w       = margin + (right - left) + margin
    val h       = margin + (bottom - top) + margin

    val draw = QuickDraw(math.ceil(w).toInt -> math.ceil(h).toInt) { g =>
      g.setColor(Color.black)
      g.translate(margin - left, margin - top)
      edges.foreach { case (p, c) =>
        // g.setColor(Color.red)
        g.draw(new Line2D.Double(p.location, c.location))
      }
      nodes.foreach { n =>
        val isBranching = edges.count(_._1 == n) > 1
        g.setColor(if (isBranching) Color.red else Color.black)
        g.fill(n.shape(if (isBranching) 3.0 else 2.0))
      }
    }

    pdflitz.Generate(pdfFile, draw, overwrite = true)
  }

  def generateDOT(overwrite: Boolean = false): Unit = {
    if (!overwrite && dotFile.isFile) {
      println(s"File '$dotFile' already exists.")
      return
    }
    val versions  = JsIO.read[Vec[Version]](jsonFile).get
    val out       = new PrintStream(dotFile)
    try {
      import out._
      println("digraph Versions {")
      // println("""  len=0.1;""")
      println("""  ranksep=0.05;""")
      println("""  node [style=filled, shape=point, fillcolor=black, fixedsize=true, width=0.3, height=0.1, fontname="Helvetica", fontsize=8, fontcolor=white];""")
      println("""  edge [arrowhead=none, len=0.1];""")
      var eliminate = 1     // don't include the main cursor
      var xpos = 0
      versions.foreach { v =>
        if (v.id == eliminate || v.parent == eliminate) {
          eliminate = v.id
        } else if (v.parent == -1) {
          println(s"""  ${v.id} [pos="${xpos * 10},0!", fillcolor=red];""")
          xpos += 1
        } else {
          println(s"  ${v.parent} -> ${v.id};")
        }
      }
      println("}")
      Console.out.println(s"Wrote '$dotFile'.")
    } finally {
      out.close()
    }
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
