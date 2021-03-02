package hex2048

import hex2048.Hex2048State.Cell
import hex2048.Hex2048State.Tile.{Empty, Filled, HasValue}
import org.scalajs.dom.raw.XMLHttpRequest
import org.scalajs.dom.ext.Ajax
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}

trait Hex2048RngService {
	type ErrorMsg = String

	def fillStateWithNextValues(state: Hex2048State, radius: Int): Future[Either[ErrorMsg, Hex2048State]]
}

class DataFromServer(serverHost: String, serverPort: Int) extends Hex2048RngService {
	import DataFromServer._

	implicit val ec = ExecutionContext.global

	def fillStateWithNextValues(state: Hex2048State, radius: Int): Future[Either[ErrorMsg, Hex2048State]] = {
		val filledCells = (state.state collect {
			case Cell(x, y, z, value: HasValue) => CellDto(x, y, z, value.value)
		})
			.asJson
			.noSpaces

		Ajax.post(s"http://$serverHost:$serverPort/$radius", Ajax.InputData.str2ajax(filledCells)) map { req =>
			val parsedResponse = decode[Array[CellDto]](req.responseText) match {
				case Left(error) => Left(s"Can't parse data from server: ${error.getMessage}")
				case Right(value) => Right(value)
			}
			parsedResponse.map(addGeneratedValuesToExistingState(state, _))
		}
	}

	def addGeneratedValuesToExistingState(state: Hex2048State, cells: Array[CellDto]): Hex2048State = {
		def update(current: Array[Cell], cells: Array[CellDto]): Array[Cell] = {
			if(cells.length < 1) current
			else {
				val newCell = cells(0)
				val index = current.indexWhere(c => (c.x, c.y, c.z) == (newCell.x, newCell.y, newCell.z))
				current.lift(index).fold(current) { oldCell =>
					require(oldCell.tile == Empty
					update(current.updated(index, Cell(newCell.x, newCell.y, newCell.z, Filled(newCell.value))), cells.drop(1))
				}
			}
		}

		state.copy(state = update(state.state, cells))
	}
}

object DataFromServer {
	final case class CellDto(x: Int, y: Int, z: Int, value: Int)
}