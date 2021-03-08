package hex2048

import cats.Monad
import cats.data.EitherT
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import hex2048.Hex2048State.Cell
import hex2048.Hex2048State.Cell.CubeCoordinate
import hex2048.Hex2048State.Direction
import hex2048.Hex2048State.Tile.Empty

import scala.concurrent.ExecutionContext

class Hex2048Game[F[_]: Monad] private (
  config: Hex2048Game.Config,
  rngService: Hex2048RngService[F],
  htmlRenderer: Hex2048HtmlRenderer[F],
  stateRef: Ref[F, Hex2048State],
) {

  implicit val ec = ExecutionContext.global

  def handleKeyPress(key: String): EitherT[F, String, Unit] = {
    key match {
      case "w" | "W" => handleCommand(Direction.North)
      case "e" | "E" => handleCommand(Direction.NorthEast)
      case "d" | "D" => handleCommand(Direction.SouthEast)
      case "s" | "S" => handleCommand(Direction.South)
      case "a" | "A" => handleCommand(Direction.SouthWest)
      case "q" | "Q" => handleCommand(Direction.NorthWest)
      case _ => EitherT.leftT("Key not recognized")
    }
  }

  def handleCommand(direction: Direction): EitherT[F, String, Unit] = {
    for {
      state <- EitherT.right(stateRef.get)
      shifted = state.shifted(direction)
      newState <-
        if (shifted.wasChanged) {
          for {
            generated <- EitherT[F, rngService.ErrorMsg, Array[Cell]](
              rngService.nextValues(shifted.value, config.radius),
            )
            withGenerated = shifted.value.withGenerated(generated)
            newState <- EitherT.right[rngService.ErrorMsg](stateRef.updateAndGet(_ => withGenerated))
          } yield newState
        } else EitherT.rightT[F, rngService.ErrorMsg](shifted.value)
      _ <- EitherT.right(htmlRenderer.drawGame(newState))
    } yield ()
  }

  def draw(): F[Unit] = {
    for {
      state <- stateRef.get
      result <- htmlRenderer.drawGame(state)
    } yield result
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
    } yield Cell(CubeCoordinate(x, y, z), Empty)
    Hex2048State(state.toArray)
  }

  def of[F[_]: Sync](
    config: Hex2048Game.Config,
    rngService: Hex2048RngService[F],
    htmlRenderer: Hex2048HtmlRenderer[F],
  ): EitherT[F, ErrorMsg, Hex2048Game[F]] = {
    val empty = initialState(config.radius)
    for {
      generated <- EitherT(rngService.nextValues(empty, config.radius))
      state = empty.withGenerated(generated)
      stateRef <- EitherT.right(Ref.of(state))
    } yield new Hex2048Game(config, rngService, htmlRenderer, stateRef)
  }
}
