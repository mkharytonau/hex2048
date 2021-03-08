package hex2048

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen
import hex2048.Hex2048State.{Cell, GameStatus, Tile}
import hex2048.Hex2048State.Cell.CubeCoordinate
import hex2048.Hex2048State.Direction.North
import hex2048.Hex2048State.Tile._
import org.scalacheck.Test.Parameters

class Hex2048StatePropertySpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  implicit val params = Parameters.default.withMinSuccessfulTests(1000)

  val tileValue: Gen[Int] = Gen.choose(1, 12).map(e => Integer.parseInt("1" + "0" * e, 2))

  val tile: Gen[Tile] = Gen.frequency(
    1 -> Empty,
    1 -> tileValue.map(Generated),
    1 -> tileValue.map(Filled),
    1 -> tileValue.map(Shifted(_, CubeCoordinate.zeros)),
    1 -> tileValue.map(Merged(_, CubeCoordinate.zeros, CubeCoordinate.zeros)),
  )

  // Generates states with real cube coordinates
  val hex2048State: Gen[Hex2048State] = for {
    radius <- Gen.choose(2, 20)
    valueRange = -(radius - 1) until radius
    coordinates = for {
      x <- valueRange
      y <- valueRange
      z <- valueRange
      if x + y + z == 0
    } yield CubeCoordinate(x, y, z)
    cells <- Gen.sequence[List[Cell], Cell](coordinates.map(c => tile.map(tile => Cell(c, tile))))
  } yield Hex2048State(cells.toArray)

  // Generates state with x = 0 for all cell to test shifting
  val hex2048StateInOneRow: Gen[Hex2048State] = for {
    radius <- Gen.choose(2, 20)
    valueRange = -(radius - 1) until radius
    coordinates = for {
      x <- valueRange.map(_ => 0)
      y <- -(radius - 1) until radius
      z <- radius - 1 until -radius by -1
      if x + y + z == 0
    } yield CubeCoordinate(x, y, z)
    cells <- Gen.sequence[List[Cell], Cell](coordinates.map(c => tile.map(tile => Cell(c, tile))))
  } yield Hex2048State(cells.toArray)

  val generatedValue: Gen[Int] = Gen.oneOf(Seq(2, 4))
  def cellsGenerator(from: Array[CubeCoordinate]): Gen[Array[Cell]] = {
    for {
      count <- Gen.choose(1, from.length)
      coordinates <- Gen.pick(count, from)
      values <- Gen.sequence[List[Int], Int](List.fill(coordinates.length)(generatedValue))
    } yield (coordinates.zip(values) map { case (coordinate, value) => Cell(coordinate, Generated(value)) }).toArray
  }

  "tiles list" should "stays the same length after shifting" in {
    forAll(hex2048State) { state =>
      state.shifted(North).value.state.length shouldBe state.state.length
    }
  }

  "shifted tiles list" should "not contain two tiles with the same values nearby" +
    "(except at least one of them was already merged)" in {
      forAll(hex2048StateInOneRow) { state =>
        val shifted = state.shifted(North).value.state.map(_.tile)
        (shifted.zip(shifted.drop(1)) collect { case (c1: HasValue, c2: HasValue) => c1 -> c2 } collect {
          case (c1 @ (Filled(_) | Shifted(_, _)), c2 @ (Filled(_) | Shifted(_, _))) if c1.value == c2.value =>
        }) shouldBe empty
      }
    }

  "shifted tiles list" should "have all Empty tiles floated to the start" in {
    forAll(hex2048StateInOneRow) { state =>
      whenever(state.state.map(_.tile).exists {
        case Empty => false
        case _ => true
      }) {
        val shifted = state.shifted(North).value.state.map(_.tile)
        val indexOfFirstNonEmpty = shifted.indexWhere {
          case Empty => false
          case _ => true
        }
        shifted.drop(indexOfFirstNonEmpty + 1) collect { case Empty => } shouldBe empty
      }
    }
  }

  "shifted tiles list" should "have Merged tiles" in {
    forAll(hex2048StateInOneRow) { state =>
      val nonEmpties = state.state.map(_.tile).collect { case t: Filled =>
        t
      }
      val cond = nonEmpties.zip(nonEmpties.drop(1)) collect {
        case (t1, t2) if t1.value == t2.value =>
      }
      whenever(cond.length > 0) {
        val shifted = state.shifted(North).value.state.map(_.tile)
        shifted.exists {
          case _: Merged => true
          case _ => false
        } shouldBe true
      }
    }
  }

  "smoothed" should "produce state with only Empty or Filled cells" in {
    forAll(hex2048State) { state =>
      (state.shifted(North).value.smoothed.state collect { case Cell(_, Shifted(_, _) | Merged(_, _, _)) =>
        1
      }) shouldBe empty
    }
  }

  "withGenerated" should "keep existing cells untouched" in {
    forAll(hex2048State) { state =>
      val coordinatesOfEmptyCells = state.state collect { case Cell(coordinate, Empty) =>
        coordinate
      }
      val cellsWithValues = state.state.collect { case c @ Cell(_, _: HasValue) =>
        c
      }
      whenever(coordinatesOfEmptyCells.nonEmpty) {
        forAll(cellsGenerator(coordinatesOfEmptyCells)) { generatedCells =>
          val updated = state.withGenerated(generatedCells)
          cellsWithValues foreach { cell =>
            val cellInNewState = updated.state.find(_.coordinate == cell.coordinate)
            cellInNewState should not be empty
            cellInNewState.get.tile shouldBe cell.tile
          }
        }
      }
    }
  }

  it should "return right number of non-empty cells" in {
    forAll(hex2048State) { state =>
      val coordinatesOfEmptyCells = state.state collect { case Cell(coordinate, Empty) =>
        coordinate
      }
      val cellsWithValuesCount = state.state.collect { case _ @Cell(_, _: HasValue) =>
      }.length
      whenever(coordinatesOfEmptyCells.nonEmpty) {
        forAll(cellsGenerator(coordinatesOfEmptyCells)) { generatedCells =>
          val updated = state.withGenerated(generatedCells)
          val cellsWithValuesCountAfterUpdate = updated.state.collect { case _ @Cell(_, _: HasValue) =>
          }.length
          cellsWithValuesCountAfterUpdate shouldBe (cellsWithValuesCount + generatedCells.length)
        }
      }
    }
  }

  "gameStatus" should "return Playing if there are empty cells" in {
    forAll(hex2048State) { state =>
      val allFilled = state.state.map(_.tile) forall {
        case _: HasValue => true
        case _ => false
      }
      whenever(!allFilled) {
        state.gameStatus match {
          case GameStatus.GameOver(_) => fail("GameStatus should be Playing.")
          case GameStatus.Playing(_) => succeed
        }
      }
    }
  }
}
