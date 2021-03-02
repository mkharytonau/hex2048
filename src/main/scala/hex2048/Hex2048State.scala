package hex2048

import hex2048.Hex2048State.Cell

import scala.annotation.tailrec
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import hex2048.Hex2048State.Tile.Empty
import hex2048.Hex2048State.Direction.North

final case class Hex2048State(state: Array[Cell]) {

  import Hex2048State._
  import Tile._

  private def shiftOneRow(source: Array[Cell]): Array[Cell] = {
    val reversed = source.reverse

    def shiftRec(current: Array[Cell], index: Int): Array[Cell] = {

      val justTiles = current.map(_.tile)

      val toShiftOption = justTiles.drop(index + 1).zipWithIndex.collectFirst { case (c: Filled, i) =>
        (c, i + index + 1)
      }
      val cellToUpdate: Option[(Int, Tile, Int)] = for {
        (toShift, oldIndex) <- toShiftOption
        placeTo = justTiles.take(oldIndex + 1).zipWithIndex.collectFirst { case (Empty, i) =>
          i
        }
        placed <- placeTo
          .map(newIndex => (newIndex, Shifted(toShift.value, oldIndex), oldIndex))
          .orElse(Some((oldIndex, toShift, oldIndex)))
        merged = justTiles.lift(placed._1 - 1) match {
          case Some(Filled(value)) if toShift.value == value =>
            Some(
              (
                placed._1 - 1,
                Merged(value + toShift.value, placed._1 - 1, oldIndex),
                oldIndex,
              ),
            )
          case Some(Shifted(value, shiftedFrom)) if toShift.value == value =>
            Some(
              (
                placed._1 - 1,
                Merged(value + toShift.value, shiftedFrom, oldIndex),
                oldIndex,
              ),
            )
          case _ => None
        }
        directive <- merged.orElse(Some(placed))
      } yield directive

      val updatedCurrent = cellToUpdate.fold(current) {
        case (newIndex, newTile, oldIndex) if (newIndex != oldIndex) =>
          val withNewCell = current.updated(newIndex, current(newIndex).copy(tile = newTile))
          withNewCell.updated(oldIndex, withNewCell(oldIndex).copy(tile = Empty))
        case _ => current
      }
      toShiftOption.fold(updatedCurrent) { case (_, oldIndex) =>
        shiftRec(updatedCurrent, oldIndex)
      }
    }

    shiftRec(reversed, -1)
      .map(cell => {
        cell.tile match {
          case s: Shifted =>
            cell.copy(tile = s.copy(fromIndex = reverseIndex(source, s.fromIndex)))
          case m: Merged =>
            cell.copy(tile =
              m.copy(
                parent1Index = reverseIndex(source, m.parent1Index),
                parent2Index = reverseIndex(source, m.parent2Index),
              ),
            )
          case _ => cell
        }
      })
      .reverse
  }

  private def reverseIndex[T](source: Array[T], indexInReversed: Int) =
    source.length - indexInReversed - 1

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
    string.split(" ") collect {
      case "_" => Cell.justTile(Empty)
      case str if isAcceptableInt(str) => Cell.justTile(Filled(str.toInt))
    }
  }

  private def getLinesToShift(direction: Direction): Iterable[Array[Cell]] = {
    import Direction._

    val grouped = direction match {
      case North => state.groupBy(_.x).mapValues(_.sortBy(-_.z))
      case NorthEast => state.groupBy(_.y).mapValues(_.sortBy(-_.z)) 
      case SouthEast => state.groupBy(_.z).mapValues(_.sortBy(_.x))
      case South => state.groupBy(_.x).mapValues(_.sortBy(_.z))
      case SouthWest => state.groupBy(_.y).mapValues(_.sortBy(_.z))
      case NorthWest => state.groupBy(_.z).mapValues(_.sortBy(-_.x))
    }

    grouped.mapValues(_.toArray).values
  }

  def shifted(direction: Direction): Hex2048State = {
    copy(state = getLinesToShift(direction).flatMap(shiftOneRow).toArray)
  }

  def smoothed: Hex2048State = {
    copy(
      state = state map {
        case Cell(x, y, z, Shifted(value, _)) => Cell(x, y, z, Filled(value))
        case Cell(x, y, z, Merged(value, _, _)) => Cell(x, y, z, Filled(value))
        case c => c
      }
    )
  }
}

object Hex2048State {

  val empty = Hex2048State(Array.empty)

  final case class Cell(
    x: Int,
    y: Int,
    z: Int,
    tile: Tile,
  ) {
    def test: Cell = Cell(0, 0, 0, Empty)
  }

  object Cell {
    def justTile(tile: Tile): Cell = Cell(0, 0, 0, tile)
  }

  trait Direction

  object Direction {
    case object North extends Direction
    case object NorthEast extends Direction
    case object SouthEast extends Direction
    case object South extends Direction
    case object SouthWest extends Direction
    case object NorthWest extends Direction
  }

  trait Tile

  object Tile {

    case object Empty extends Tile

    sealed trait HasValue extends Tile { def value: Int }

    final case class Filled(value: Int) extends HasValue
    final case class Shifted(value: Int, fromIndex: Int) extends HasValue
    final case class Merged(value: Int, parent1Index: Int, parent2Index: Int) extends HasValue
  }
}
