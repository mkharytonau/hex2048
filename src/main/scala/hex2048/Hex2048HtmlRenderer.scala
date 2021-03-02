package hex2048

import hex2048.Hex2048State.Cell
import hex2048.Hex2048State.Tile.{Empty, HasValue}
import org.scalajs.dom.document

import scala.math._

class Hex2048HtmlRenderer(htmlContainerId: String, gameXc: Int, gameYc: Int, radius: Int) {

  def polygonPoints(r: Double): String = {
    (0 to 300 by 60).map(_ * scala.math.Pi / 180).map(rad => s"${r + r * cos(rad)},${r * sin(Pi / 3) - r * sin(rad)}").mkString(" ")
  }

  def cubeToOddq(cube: (Int, Int, Int)): (Int, Int) = {
    cube match {
      case (x, y, z) => (x, z + (x - (x & 1)) / 2)
    }
  }

  def oddqToAbsolute(xc: Double, yc: Double, r: Double, oddq: (Int, Int)): (Double, Double) = {
    oddq match {
      case (x, y) => (xc + x * r * 1.5, yc + r * sin(Pi / 3) * (2 * y + (x & 1)))
    }
  }

  def draw(state: Array[Hex2048State.Cell]): Unit = {
    val gameContainer = document.getElementById(htmlContainerId)

    while(gameContainer.hasChildNodes()) {
      val child = gameContainer.firstChild
      gameContainer.removeChild(child)
    }

    val elements = (state.map { case Cell(x, y, z, tile) =>
      val oddq = cubeToOddq((x, y, z))
      val (xc, yc) = oddqToAbsolute(gameXc, gameYc, radius, oddq)
      val polygonPts = polygonPoints(radius)

      val div = document.createElement("div")
      div.setAttribute("data-x", x.toString)
      div.setAttribute("data-y", y.toString)
      div.setAttribute("data-z", z.toString)
      div.setAttribute("data-value", {
        tile match {
          case Empty => "0"
          case withValue: HasValue => withValue.value.toString
        }})
      div.setAttribute("style", s"position: absolute; left: ${xc - radius}px; top: ${yc - radius * sin(Pi / 3)}px")

      val svg = document.createElementNS("http://www.w3.org/2000/svg", "svg")
      svg.setAttribute("height", (2 * radius * sin(Pi / 3)).toString)
      svg.setAttribute("width", (2 * radius).toString)

      val tileValue = tile match {
        case Empty => ""
        case withValue: HasValue => withValue.value.toString
      }
      val polygon = document.createElementNS("http://www.w3.org/2000/svg", "polygon")
      polygon.setAttribute("points", polygonPts)
      polygon.setAttribute("class", s"""tile ${if (tileValue.nonEmpty) s"tile-$tileValue" else ""}""")
      val text = document.createElementNS("http://www.w3.org/2000/svg", "text")
      text.setAttribute("x", "50%")
      text.setAttribute("y", "50%")
      text.setAttribute("text-anchor", "middle")
      text.setAttribute("dominant-baseline", "middle")
      text.textContent = {
        tile match {
          case Empty => ""
          case withValue: HasValue => withValue.value.toString
        }}

      svg.appendChild(polygon)
      svg.appendChild(text)
      div.appendChild(svg)

      div
    })

    elements foreach gameContainer.appendChild
  }

}
