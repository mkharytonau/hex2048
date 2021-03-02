package hex2048

import hex2048.Hex2048State.{Cell, Direction}
import hex2048.Hex2048State.Tile.Empty

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.impl.Promise

class Hex2048Game private (
  config: Hex2048Game.Config,
  rngService: Hex2048RngService,
  htmlRenderer: Hex2048HtmlRenderer,
  @volatile private var state: Hex2048State,
) {

  implicit val ec = ExecutionContext.global

  def handleKeyPress(key: String): Future[Either[String, Unit]] = {
    key match {
      case "w" | "W" => handleCommand(Direction.North)
      case "e" | "E" => handleCommand(Direction.NorthEast)
      case "d" | "D" => handleCommand(Direction.SouthEast)
      case "s" | "S" => handleCommand(Direction.South)
      case "a" | "A" => handleCommand(Direction.SouthWest)
      case "q" | "Q" => handleCommand(Direction.NorthWest)
      case _ => Future.successful(Left("Key not recognized"))
    }
  }

  def handleCommand(direction: Direction): Future[Either[String, Unit]] = {
    val smoothed = state.smoothed
    val shifted = smoothed.shifted(direction)
    val withNewlyGenerated = rngService.fillStateWithNextValues(shifted, config.radius)
    withNewlyGenerated.map(_.map(newState => {
      htmlRenderer.draw(newState.state)
      state = newState
    }))
  }

  def draw(): Unit = {
    htmlRenderer.draw(state.state)
  }
}

object Hex2048Game {

  type ErrorMsg = String

  final case class Config(
    radius: Int,
  )

  private def initialState(radius: Int): Hex2048State = {
    val valueRange = -(radius - 1) until radius
    val state = for {
      x <- valueRange
      y <- valueRange
      z <- valueRange
      /*
			Taking just points that lies on this plane
			https://www.researchgate.net/figure/The-graph-of-the-x-y-z-0-plane_fig2_50218038
       */
      if x + y + z == 0
    } yield Cell(x, y, z, Empty)
    Hex2048State(state.toArray)
  }

  implicit val ec = ExecutionContext.global

  def of(
    config: Hex2048Game.Config,
    rngService: Hex2048RngService,
    htmlRenderer: Hex2048HtmlRenderer,
  ): Future[Either[ErrorMsg, Hex2048Game]] = {
    val future = rngService.fillStateWithNextValues(initialState(config.radius), config.radius)

    future map {
      case Left(error) => Left(s"Error occurred while initializing new Hex2048 game, error: $error")
      case Right(state) => Right(new Hex2048Game(config, rngService, htmlRenderer, state))
    }
  }

}
