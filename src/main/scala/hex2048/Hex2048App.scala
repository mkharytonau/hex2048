package hex2048

import org.scalajs.dom
import org.scalajs.dom.raw.XMLHttpRequest
import org.scalajs.dom
import org.scalajs.dom.document

import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.svg.SVG

import scala.concurrent.ExecutionContext
import scala.math._

object Hex2048App {

  implicit val ec = ExecutionContext.global

  def main(args: Array[String]): Unit = {
    Hex2048Game.of(
      config = Hex2048Game.Config(
        radius = 3,
      ),
      rngService = new DataFromServer(
        serverHost = "51.15.207.127",
        serverPort = 13337,
      ),
      htmlRenderer = new Hex2048HtmlRenderer(
        htmlContainerId = "game-container",
        gameXc = 700,
        gameYc = 500,
        radius = 50,
      ),
    ) map {
      case Left(error) => println(s"Unable to create game: error: $error")
      case Right(game) =>
        game.draw()
        document.addEventListener("keydown", (e: dom.KeyboardEvent) => game.handleKeyPress(e.key).map {
        case Left(error) => println(s"Handle key(${e.key}) press was unsuccessful: error: $error")
        case Right(_) => println(s"Handle key press was successful")
      })
    }
  }
}
