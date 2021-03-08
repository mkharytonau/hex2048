package hex2048

import cats.effect.Async
import cats.effect.ContextShift
import cats.syntax.all._
import hex2048.Hex2048State.Cell
import hex2048.Hex2048State.Cell.CubeCoordinate
import hex2048.Hex2048State.Tile.Generated
import hex2048.Hex2048State.Tile.HasValue
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom.ext.Ajax

import scala.concurrent.ExecutionContext

trait Hex2048RngService[F[_]] {
  type ErrorMsg = String

  def nextValues(state: Hex2048State, radius: Int): F[Either[ErrorMsg, Array[Cell]]]
}

class DataFromServer[F[_]: Async: ContextShift](serverHost: String, serverPort: Int) extends Hex2048RngService[F] {
  import DataFromServer._

  implicit val ec = ExecutionContext.global

  def nextValues(state: Hex2048State, radius: Int): F[Either[ErrorMsg, Array[Cell]]] = {
    val filledCells = (state.state collect { case Cell(CubeCoordinate(x, y, z), value: HasValue) =>
      CellDto(x, y, z, value.value)
    }).asJson.noSpaces

    Async
      .fromFuture(
        implicitly[Async[F]].delay(
          Ajax.post(s"http://$serverHost:$serverPort/$radius", Ajax.InputData.str2ajax(filledCells)),
        ),
      )
      .map { req =>
        val parsedResponse = decode[Array[CellDto]](req.responseText) match {
          case Left(error) => Left(s"Can't parse data from server: ${error.getMessage}")
          case Right(value) =>
            Right(value.map(cellDto => Cell(CubeCoordinate(cellDto.x, cellDto.y, cellDto.z), Generated(cellDto.value))))
        }
        parsedResponse
      }
  }
}

object DataFromServer {
  final case class CellDto(x: Int, y: Int, z: Int, value: Int)
}
