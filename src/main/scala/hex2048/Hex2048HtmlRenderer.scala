package hex2048

import cats.Applicative
import hex2048.Hex2048State.Cell
import hex2048.Hex2048State.Cell.CubeCoordinate
import hex2048.Hex2048State.Tile.Empty
import hex2048.Hex2048State.Tile.HasValue
import org.scalajs.dom.document

import scala.math._

class Hex2048HtmlRenderer[F[_]: Applicative](htmlContainerId: String, gameXc: Double, gameYc: Double, radius: Int) {
  import Hex2048HtmlRenderer.Constants

  def polygonPoints(r: Double): String = {
    (0 to 300 by 60)
      .map(_ * scala.math.Pi / 180)
      .map(rad => s"${r + r * cos(rad)},${r * Constants.sin60 - r * sin(rad)}")
      .mkString(" ")
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

  def drawGame(state: Hex2048State): F[Unit] = implicitly[Applicative[F]] pure {
    val gameContainer = document.getElementById(htmlContainerId)

    while (gameContainer.hasChildNodes()) {
      val child = gameContainer.firstChild
      gameContainer.removeChild(child)
    }

    val gameStatus = document.createElement("div")
    gameStatus.textContent = "Game Status: "
    val gameStatusSpan = document.createElement("span")
    gameStatusSpan.setAttribute("data-status", state.gameStatus.toString)
    gameStatusSpan.textContent = state.gameStatus.toString
    gameStatus.appendChild(gameStatusSpan)
    gameContainer.appendChild(gameStatus)

    val elements = (state.state.map { case Cell(CubeCoordinate(x, y, z), tile) =>
      val oddq = cubeToOddq((x, y, z))
      val (xc, yc) = oddqToAbsolute(gameXc, gameYc, radius, oddq)
      val polygonPts = polygonPoints(radius)

      val div = document.createElement("div")
      div.setAttribute("data-x", x.toString)
      div.setAttribute("data-y", y.toString)
      div.setAttribute("data-z", z.toString)
      div.setAttribute(
        "data-value", {
          tile match {
            case Empty => "0"
            case withValue: HasValue => withValue.value.toString
          }
        },
      )
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
        }
      }

      svg.appendChild(polygon)
      svg.appendChild(text)
      div.appendChild(svg)

      div
    })

    elements foreach gameContainer.appendChild
  }

  def drawInitialPage(): Unit = {
//    val inputLabel = document.createElement("label")
//    inputLabel.setAttribute("for", "gameRadius")
//    inputLabel.textContent = "Game radius:"
//
//    val gameRadiusInput = document.createElement("")

  }

}

object Hex2048HtmlRenderer {
  object Constants {
    import scala.math._
    val sin60: Double = sin(Pi / 3)
  }
}
