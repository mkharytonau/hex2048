package hex2048

import hex2048.Hex2048State.Cell
import hex2048.Hex2048State.Cell.CubeCoordinate
import hex2048.Hex2048State.Tile._

import scala.util.Try

final case class Hex2048State(state: Array[Cell]) {

  import Hex2048State._
  import Tile._

  private def shiftOneRow(source: Array[Cell]): UpdateResult[Array[Cell]] = {
    val reversed = source.reverse

    def shiftRec(current: Array[Cell], index: Int, changedCurrent: Boolean): UpdateResult[Array[Cell]] = {
      val toShiftOption =
        current.drop(index + 1).zipWithIndex.collectFirst { case (Cell(coordinate, f @ Filled(_)), i) =>
          (f, i + index + 1, coordinate)
        }
      val cellToUpdate: Option[(Int, Tile, Int)] = for {
        (toShift, oldIndex, oldCoordinate) <- toShiftOption
        placeTo = current.take(oldIndex + 1).zipWithIndex.collectFirst { case (Cell(_, Empty), i) =>
          i
        }
        placed <- placeTo
          .map(newIndex => (newIndex, Shifted(toShift.value, oldCoordinate), oldIndex))
          .orElse(Some((oldIndex, toShift, oldIndex)))
        merged = current.lift(placed._1 - 1) match {
          case Some(Cell(coordinate, Filled(value))) if toShift.value == value =>
            Some(
              (
                placed._1 - 1,
                Merged(value + toShift.value, coordinate, oldCoordinate),
                oldIndex,
              ),
            )
          case Some(Cell(_, Shifted(value, shiftedFrom))) if toShift.value == value =>
            Some(
              (
                placed._1 - 1,
                Merged(value + toShift.value, shiftedFrom, oldCoordinate),
                oldIndex,
              ),
            )
          case _ => None
        }
        directive <- merged.orElse(Some(placed))
      } yield directive

      val updatedCurrent: UpdateResult[Array[Cell]] = cellToUpdate.fold(UpdateResult(current, wasChanged = false)) {
        case (newIndex, newTile, oldIndex) if newIndex != oldIndex =>
          val withNewCell = current.updated(newIndex, current(newIndex).copy(tile = newTile))
          UpdateResult(
            value = withNewCell.updated(oldIndex, withNewCell(oldIndex).copy(tile = Empty)),
            wasChanged = true,
          )
        case _ => UpdateResult(current, wasChanged = false)
      }
      toShiftOption.fold(updatedCurrent.copy(wasChanged = changedCurrent || updatedCurrent.wasChanged)) {
        case (_, oldIndex, _) =>
          shiftRec(updatedCurrent.value, oldIndex, changedCurrent || updatedCurrent.wasChanged)
      }
    }

    val shifted = shiftRec(reversed, -1, changedCurrent = false)
    shifted.copy(value = shifted.value.reverse)
  }

  private def getLinesToShift(direction: Direction): Iterable[Array[Cell]] = {
    import Direction._

    val grouped = direction match {
      case North => state.groupBy(_.coordinate.x).mapValues(_.sortBy(_.coordinate.y))
      case NorthEast => state.groupBy(_.coordinate.y).mapValues(_.sortBy(-_.coordinate.z))
      case SouthEast => state.groupBy(_.coordinate.z).mapValues(_.sortBy(_.coordinate.x))
      case South => state.groupBy(_.coordinate.x).mapValues(_.sortBy(-_.coordinate.y))
      case SouthWest => state.groupBy(_.coordinate.y).mapValues(_.sortBy(_.coordinate.z))
      case NorthWest => state.groupBy(_.coordinate.z).mapValues(_.sortBy(-_.coordinate.x))
    }

    grouped.mapValues(_.toArray).values
  }

  def shifted(direction: Direction): UpdateResult[Hex2048State] = {
    val shiftedLines = smoothed
      .getLinesToShift(direction)
      .map(shiftOneRow)
    UpdateResult(
      value = copy(state = shiftedLines.flatMap(_.value).toArray),
      wasChanged = shiftedLines.map(_.wasChanged).foldLeft(false)(_ || _),
    )
  }

  def smoothed: Hex2048State = {
    copy(
      state = state map {
        case Cell(coordinate, Generated(value)) => Cell(coordinate, Filled(value))
        case Cell(coordinate, Shifted(value, _)) => Cell(coordinate, Filled(value))
        case Cell(coordinate, Merged(value, _, _)) => Cell(coordinate, Filled(value))
        case c => c
      },
    )
  }

  def withGenerated(cells: Array[Cell]): Hex2048State = {
    def update(current: Array[Cell], cells: Array[Cell]): Array[Cell] = {
      if (cells.length < 1) current
      else {
        val newCell = cells(0)
        val index = current.indexWhere(c => (c.x, c.y, c.z) == (newCell.x, newCell.y, newCell.z))
        current.lift(index).fold(current) { oldCell =>
          require(oldCell.tile == Empty)
          update(
            current.updated(index, newCell),
            cells.drop(1),
          )
        }
      }
    }

    copy(state = update(state, cells))
  }

  def gameStatus: GameStatus = {
    val allFilled = state.map(_.tile) forall {
      case _: HasValue => true
      case _ => false
    }
    if (!allFilled) GameStatus.Playing(result)
    else {
      val shiftingChangesNothing = Direction.All.map(shifted).map(_.wasChanged).forall(!_)
      if (shiftingChangesNothing) GameStatus.GameOver(result)
      else GameStatus.Playing(result)
    }
  }

  def result: Int = {
    state
      .map(cell => {
        cell.tile match {
          case Tile.Empty => 0
          case withValue: HasValue => withValue.value
        }
      })
      .maxOption
      .getOrElse(0)
  }

  override def toString: String = s"Hex2048State(${state.mkString(",")})"
}

object Hex2048State {

  val empty: Hex2048State = Hex2048State(Array.empty)

  // Used for testing purposes only
  def oneTestRow(str: String): Hex2048State = Hex2048State(parseString(str))

  final case class UpdateResult[T](value: T, wasChanged: Boolean)

  final case class Cell(
    coordinate: CubeCoordinate,
    tile: Tile,
  ) {
    def x: Int = coordinate.x
    def y: Int = coordinate.y
    def z: Int = coordinate.z
  }

  object Cell {

    final case class CubeCoordinate(x: Int, y: Int, z: Int)

    object CubeCoordinate {
      // Used only for tests
      def y(y: Int): CubeCoordinate = zeros.copy(y = y)

      def zeros: CubeCoordinate = CubeCoordinate(0, 0, 0)
    }
  }

  trait Direction

  object Direction {
    case object North extends Direction
    case object NorthEast extends Direction
    case object SouthEast extends Direction
    case object South extends Direction
    case object SouthWest extends Direction
    case object NorthWest extends Direction

    val All: Set[Direction] = Set(
      North,
      NorthEast,
      SouthEast,
      South,
      SouthWest,
      NorthWest,
    )
  }

  trait Tile

  object Tile {

    case object Empty extends Tile

    sealed trait HasValue extends Tile { def value: Int }

    final case class Generated(value: Int) extends HasValue
    final case class Filled(value: Int) extends HasValue
    final case class Shifted(value: Int, from: CubeCoordinate) extends HasValue
    final case class Merged(value: Int, parent1: CubeCoordinate, parent2: CubeCoordinate) extends HasValue
  }

  sealed trait GameStatus { def result: Int }

  object GameStatus {
    final case class GameOver(result: Int) extends GameStatus {
      override def toString: String = "game-over"
    }
    final case class Playing(result: Int) extends GameStatus {
      override def toString: String = "playing"
    }
  }

  /*
  Method used in tests, will return cells with
    - x = 0 to capture all cells when shifting to North
    - y = index in array to test `from` in Shifted and `parent1`, `parent2` in Merged
    - z = 0 not used in tests
   */
  def parseString(string: String): Array[Cell] = {
    def isAcceptableInt(str: String): Boolean = {
      val mayBeAcceptable = for {
        integerValue <- Try(str.toInt)
        powerOfTwo <- Try {
          if (integerValue.toBinaryString.count(_ == '1') == 1)
            integerValue
          else
            throw new IllegalArgumentException(
              s"Value $integerValue is not a power of 2.",
            )
        }
      } yield powerOfTwo
      mayBeAcceptable.isSuccess
    }
    string.split(" ").zipWithIndex collect {
      case ("_", i) => Cell(CubeCoordinate(0, i, 0), Empty)
      case (str, i) if isAcceptableInt(str) => Cell(CubeCoordinate(0, i, 0), Filled(str.toInt))
    }
  }
}
