import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.extensions.ChCode

import scala.collection.mutable
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object BugRepro extends JSApp {

  @JSExport
  override def main(): Unit = {

    val parent = dom.document.createElement("div")
    dom.document.body.appendChild(parent)

    def measureChars(chars: Traversable[Int]): Seq[(Int, Double)] = {
      val container = dom.document.createElement("div")
      def createSpan = document.createElement("span")

      val spansWithChar = chars.map { char =>
        val spanEl = createSpan
        spanEl.textContent = String.valueOf(char.toChar)
        (char, spanEl)
      }
      spansWithChar.foreach { case (ch, s) =>
        container.appendChild(s)
      }

      parent.appendChild(container)

      val result = spansWithChar.map { case (ch, s) =>
        (ch, s.getBoundingClientRect().width)
      }.toSeq
      parent.removeChild(container)
      result
    }

    val defaults = ChCode.shiftableKey2Char.values.map ( shift =>
      List(shift(false), shift(true))
    ).flatten.++(ChCode.key2char.values)
    val result =  mutable.Map[Int, Double]() ++ measureChars(defaults)

    result.foreach(println(_))
  }

}
