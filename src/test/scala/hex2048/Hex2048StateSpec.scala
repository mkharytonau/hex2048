package hex2048

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import Hex2048State.{Cell, Direction, GameStatus}
import Hex2048State.Tile._
import hex2048.Hex2048State.Cell.CubeCoordinate
import hex2048.Hex2048State.Direction._

class Hex2048StateSpec extends AnyWordSpec with Matchers {

  val hex2048: Hex2048State = Hex2048State.empty

  "Hex2048State.oneTestRow" should {
    "not fail on empty string" in {
      Hex2048State.oneTestRow("").state shouldBe Array.empty
    }

    "parse one element string" in {
      Hex2048State.oneTestRow("1").state.map(_.tile) shouldBe Array(Filled(1))
    }

    "recognize '_' as Empty tile" in {
      Hex2048State.oneTestRow("_").state.map(_.tile) shouldBe Array(Empty)
    }

    "take into account just powers of 2" in {
      Hex2048State.oneTestRow("1 2 3 4 5").state.map(_.tile) shouldBe Array(
        Filled(1),
        Filled(2),
        Filled(4),
      )
    }

    "parse '_' along with numbers and have x = 0 and y = index for all cells" in {
      val state = Hex2048State.oneTestRow("_ 2 _ 4 _ _ 8").state

      // Testing this because in the test below will need be sure that
      // all passed tiles will be used when shifting to North
      state.zipWithIndex.foreach { case (Cell(CubeCoordinate(x, y, _), _), i) =>
        x shouldBe 0
        y shouldBe i
      }

      state.map(_.tile) shouldBe Array(
        Empty,
        Filled(2),
        Empty,
        Filled(4),
        Empty,
        Empty,
        Filled(8),
      )
    }
  }

  "shifted" should {
    "correctly shift empty list" in {
      Hex2048State.oneTestRow("").shifted(North).value.state shouldBe Array.empty
    }

    "correctly shift '2'" in {
      Hex2048State.oneTestRow("2").shifted(North).value.state.map(_.tile) shouldBe Array(Filled(2))
    }

    "correctly shift '_ 2'" in {
      Hex2048State.oneTestRow("_ 2").shifted(North).value.state.map(_.tile) shouldBe Array(Empty, Filled(2))
    }

    "correctly shift '2 2'" in {
      Hex2048State.oneTestRow("2 2").shifted(North).value.state.map(_.tile) shouldBe Array(
        Empty,
        Merged(4, CubeCoordinate.y(1), CubeCoordinate.y(0)),
      )
    }

    "correctly shift '2 2 2'" in {
      Hex2048State.oneTestRow("2 2 2").shifted(North).value.state.map(_.tile) shouldBe Array(
        Empty,
        Shifted(2, CubeCoordinate.y(0)),
        Merged(4, CubeCoordinate.y(2), CubeCoordinate.y(1)),
      )
    }

    "correctly shift '2 2 2 _'" in {
      Hex2048State.oneTestRow("2 2 2 _").shifted(North).value.state.map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Shifted(2, CubeCoordinate.y(0)),
        Merged(4, CubeCoordinate.y(2), CubeCoordinate.y(1)),
      )
    }

    "correctly shift '_ 2 2 _'" in {
      Hex2048State.oneTestRow("_ 2 2 _").shifted(North).value.state.map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Empty,
        Merged(4, CubeCoordinate.y(2), CubeCoordinate.y(1)),
      )
    }

    "correctly shift '_ _ _ _'" in {
      Hex2048State.oneTestRow("_ _ _ _").shifted(North).value.state.map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Empty,
        Empty,
      )
    }

    "correctly shift '16 _ _ 16 2 8 4 2 2 _'" in {
      Hex2048State.oneTestRow("16 _ _ 16 2 8 4 2 2 _").shifted(North).value.state.map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Empty,
        Empty,
        Empty,
        Merged(32, CubeCoordinate.y(3), CubeCoordinate.y(0)),
        Shifted(2, CubeCoordinate.y(4)),
        Shifted(8, CubeCoordinate.y(5)),
        Shifted(4, CubeCoordinate.y(6)),
        Merged(4, CubeCoordinate.y(8), CubeCoordinate.y(7)),
      )
    }

    "correctly shift entire game matrix on all directions" in {

      // Example from
      // https://github.com/evolution-gaming/typescript-bootcamp/tree/main/assigment/hex2048#rules
      val hex2048State = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Empty),
          Cell(CubeCoordinate(-1, 2, -1), Empty),
          Cell(CubeCoordinate(0, 2, -2), Empty),
          Cell(CubeCoordinate(-2, 1, 1), Empty),
          Cell(CubeCoordinate(-1, 1, 0), Filled(4)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(1, 1, -2), Empty),
          Cell(CubeCoordinate(-2, 0, 2), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Filled(4)),
          Cell(CubeCoordinate(0, 0, 0), Filled(2)),
          Cell(CubeCoordinate(1, 0, -1), Filled(4)),
          Cell(CubeCoordinate(2, 0, -2), Empty),
          Cell(CubeCoordinate(-1, -1, 2), Empty),
          Cell(CubeCoordinate(0, -1, 1), Filled(2)),
          Cell(CubeCoordinate(1, -1, 0), Filled(2)),
          Cell(CubeCoordinate(2, -1, -1), Empty),
          Cell(CubeCoordinate(0, -2, 2), Filled(2)),
          Cell(CubeCoordinate(1, -2, 1), Empty),
          Cell(CubeCoordinate(2, -2, 0), Empty),
        ),
      )

      val afterNorthShift = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Shifted(2, CubeCoordinate(-2, 0, 2))),
          Cell(CubeCoordinate(-1, 2, -1), Merged(8, CubeCoordinate(-1, 1, 0), CubeCoordinate(-1, 0, 1))),
          Cell(CubeCoordinate(0, 2, -2), Merged(4, CubeCoordinate(0, 1, -1), CubeCoordinate(0, 0, 0))),
          Cell(CubeCoordinate(-2, 1, 1), Empty),
          Cell(CubeCoordinate(-1, 1, 0), Empty),
          Cell(CubeCoordinate(0, 1, -1), Merged(4, CubeCoordinate(0, -1, 1), CubeCoordinate(0, -2, 2))),
          Cell(CubeCoordinate(1, 1, -2), Shifted(4, CubeCoordinate(1, 0, -1))),
          Cell(CubeCoordinate(-2, 0, 2), Empty),
          Cell(CubeCoordinate(-1, 0, 1), Empty),
          Cell(CubeCoordinate(0, 0, 0), Empty),
          Cell(CubeCoordinate(1, 0, -1), Shifted(2, CubeCoordinate(1, -1, 0))),
          Cell(CubeCoordinate(2, 0, -2), Empty),
          Cell(CubeCoordinate(-1, -1, 2), Empty),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
          Cell(CubeCoordinate(2, -1, -1), Empty),
          Cell(CubeCoordinate(0, -2, 2), Empty),
          Cell(CubeCoordinate(1, -2, 1), Empty),
          Cell(CubeCoordinate(2, -2, 0), Empty),
        ),
      )

      val afterNorthEastShift = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Empty),
          Cell(CubeCoordinate(-1, 2, -1), Empty),
          Cell(CubeCoordinate(0, 2, -2), Empty),
          Cell(CubeCoordinate(-2, 1, 1), Empty),
          Cell(CubeCoordinate(-1, 1, 0), Empty),
          Cell(CubeCoordinate(0, 1, -1), Shifted(4, CubeCoordinate(-1, 1, 0))),
          Cell(CubeCoordinate(1, 1, -2), Shifted(2, CubeCoordinate(0, 1, -1))),
          Cell(CubeCoordinate(-2, 0, 2), Empty),
          Cell(CubeCoordinate(-1, 0, 1), Shifted(2, CubeCoordinate(-2, 0, 2))),
          Cell(CubeCoordinate(0, 0, 0), Shifted(4, CubeCoordinate(-1, 0, 1))),
          Cell(CubeCoordinate(1, 0, -1), Shifted(2, CubeCoordinate(0, 0, 0))),
          Cell(CubeCoordinate(2, 0, -2), Shifted(4, CubeCoordinate(1, 0, -1))),
          Cell(CubeCoordinate(-1, -1, 2), Empty),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
          Cell(CubeCoordinate(2, -1, -1), Merged(4, CubeCoordinate(1, -1, 0), CubeCoordinate(0, -1, 1))),
          Cell(CubeCoordinate(0, -2, 2), Empty),
          Cell(CubeCoordinate(1, -2, 1), Empty),
          Cell(CubeCoordinate(2, -2, 0), Shifted(2, CubeCoordinate(0, -2, 2))),
        ),
      )

      val afterSouthEastShift = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Empty),
          Cell(CubeCoordinate(-1, 2, -1), Empty),
          Cell(CubeCoordinate(0, 2, -2), Empty),
          Cell(CubeCoordinate(-2, 1, 1), Empty),
          Cell(CubeCoordinate(-1, 1, 0), Empty),
          Cell(CubeCoordinate(0, 1, -1), Empty),
          Cell(CubeCoordinate(1, 1, -2), Empty),
          Cell(CubeCoordinate(-2, 0, 2), Empty),
          Cell(CubeCoordinate(-1, 0, 1), Empty),
          Cell(CubeCoordinate(0, 0, 0), Empty),
          Cell(CubeCoordinate(1, 0, -1), Shifted(2, CubeCoordinate(0, 1, -1))),
          Cell(CubeCoordinate(2, 0, -2), Empty),
          Cell(CubeCoordinate(-1, -1, 2), Empty),
          Cell(CubeCoordinate(0, -1, 1), Shifted(4, CubeCoordinate(-1, 0, 1))),
          Cell(CubeCoordinate(1, -1, 0), Shifted(4, CubeCoordinate(-1, 1, 0))),
          Cell(CubeCoordinate(2, -1, -1), Shifted(4, CubeCoordinate(1, 0, -1))),
          Cell(CubeCoordinate(0, -2, 2), Merged(4, CubeCoordinate(0, -2, 2), CubeCoordinate(-2, 0, 2))),
          Cell(CubeCoordinate(1, -2, 1), Shifted(2, CubeCoordinate(0, -1, 1))),
          Cell(CubeCoordinate(2, -2, 0), Merged(4, CubeCoordinate(1, -1, 0), CubeCoordinate(0, 0, 0))),
        ),
      )

      val afterSouthShift = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Empty),
          Cell(CubeCoordinate(-1, 2, -1), Empty),
          Cell(CubeCoordinate(0, 2, -2), Empty),
          Cell(CubeCoordinate(-2, 1, 1), Empty),
          Cell(CubeCoordinate(-1, 1, 0), Empty),
          Cell(CubeCoordinate(0, 1, -1), Empty),
          Cell(CubeCoordinate(1, 1, -2), Empty),
          Cell(CubeCoordinate(-2, 0, 2), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Empty),
          Cell(CubeCoordinate(0, 0, 0), Empty),
          Cell(CubeCoordinate(1, 0, -1), Empty),
          Cell(CubeCoordinate(2, 0, -2), Empty),
          Cell(CubeCoordinate(-1, -1, 2), Merged(8, CubeCoordinate(-1, 0, 1), CubeCoordinate(-1, 1, 0))),
          Cell(CubeCoordinate(0, -1, 1), Merged(4, CubeCoordinate(0, 0, 0), CubeCoordinate(0, 1, -1))),
          Cell(CubeCoordinate(1, -1, 0), Shifted(4, CubeCoordinate(1, 0, -1))),
          Cell(CubeCoordinate(2, -1, -1), Empty),
          Cell(CubeCoordinate(0, -2, 2), Merged(4, CubeCoordinate(0, -2, 2), CubeCoordinate(0, -1, 1))),
          Cell(CubeCoordinate(1, -2, 1), Shifted(2, CubeCoordinate(1, -1, 0))),
          Cell(CubeCoordinate(2, -2, 0), Empty),
        ),
      )

      val afterSouthWestShift = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Empty),
          Cell(CubeCoordinate(-1, 2, -1), Empty),
          Cell(CubeCoordinate(0, 2, -2), Empty),
          Cell(CubeCoordinate(-2, 1, 1), Shifted(4, CubeCoordinate(-1, 1, 0))),
          Cell(CubeCoordinate(-1, 1, 0), Shifted(2, CubeCoordinate(0, 1, -1))),
          Cell(CubeCoordinate(0, 1, -1), Empty),
          Cell(CubeCoordinate(1, 1, -2), Empty),
          Cell(CubeCoordinate(-2, 0, 2), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Filled(4)),
          Cell(CubeCoordinate(0, 0, 0), Filled(2)),
          Cell(CubeCoordinate(1, 0, -1), Filled(4)),
          Cell(CubeCoordinate(2, 0, -2), Empty),
          Cell(CubeCoordinate(-1, -1, 2), Merged(4, CubeCoordinate(0, -1, 1), CubeCoordinate(1, -1, 0))),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
          Cell(CubeCoordinate(2, -1, -1), Empty),
          Cell(CubeCoordinate(0, -2, 2), Filled(2)),
          Cell(CubeCoordinate(1, -2, 1), Empty),
          Cell(CubeCoordinate(2, -2, 0), Empty),
        ),
      )

      val afterNorthWestShift = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Shifted(4, CubeCoordinate(-1, 1, 0))),
          Cell(CubeCoordinate(-1, 2, -1), Shifted(2, CubeCoordinate(0, 1, -1))),
          Cell(CubeCoordinate(0, 2, -2), Empty),
          Cell(CubeCoordinate(-2, 1, 1), Shifted(4, CubeCoordinate(-1, 0, 1))),
          Cell(CubeCoordinate(-1, 1, 0), Merged(4, CubeCoordinate(0, 0, 0), CubeCoordinate(1, -1, 0))),
          Cell(CubeCoordinate(0, 1, -1), Shifted(4, CubeCoordinate(1, 0, -1))),
          Cell(CubeCoordinate(1, 1, -2), Empty),
          Cell(CubeCoordinate(-2, 0, 2), Merged(4, CubeCoordinate(-2, 0, 2), CubeCoordinate(0, -2, 2))),
          Cell(CubeCoordinate(-1, 0, 1), Shifted(2, CubeCoordinate(0, -1, 1))),
          Cell(CubeCoordinate(0, 0, 0), Empty),
          Cell(CubeCoordinate(1, 0, -1), Empty),
          Cell(CubeCoordinate(2, 0, -2), Empty),
          Cell(CubeCoordinate(-1, -1, 2), Empty),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
          Cell(CubeCoordinate(2, -1, -1), Empty),
          Cell(CubeCoordinate(0, -2, 2), Empty),
          Cell(CubeCoordinate(1, -2, 1), Empty),
          Cell(CubeCoordinate(2, -2, 0), Empty),
        ),
      )

      val NorthShift = hex2048State.shifted(North)
      NorthShift.value.state.sortBy(cell => (-cell.y, -cell.z)) shouldBe afterNorthShift.state
      NorthShift.wasChanged shouldBe true

      val NorthEastShift = hex2048State.shifted(NorthEast)
      NorthEastShift.value.state.sortBy(cell => (-cell.y, -cell.z)) shouldBe afterNorthEastShift.state
      NorthEastShift.wasChanged shouldBe true

      val SouthEastShift = hex2048State.shifted(SouthEast)
      SouthEastShift.value.state.sortBy(cell => (-cell.y, -cell.z)) shouldBe afterSouthEastShift.state
      SouthEastShift.wasChanged shouldBe true

      val SouthShift = hex2048State.shifted(South)
      SouthShift.value.state.sortBy(cell => (-cell.y, -cell.z)) shouldBe afterSouthShift.state
      SouthShift.wasChanged shouldBe true

      val SouthWestShift = hex2048State.shifted(SouthWest)
      SouthWestShift.value.state.sortBy(cell => (-cell.y, -cell.z)) shouldBe afterSouthWestShift.state
      SouthWestShift.wasChanged shouldBe true

      val NorthWestShift = hex2048State.shifted(NorthWest)
      NorthWestShift.value.state.sortBy(cell => (-cell.y, -cell.z)) shouldBe afterNorthWestShift.state
      NorthWestShift.wasChanged shouldBe true
    }

    "have correct wasChanged flag in UpdateResult" in {
      Hex2048State.oneTestRow("").shifted(North).wasChanged shouldBe false
      Hex2048State.oneTestRow("_ _ _ _").shifted(North).wasChanged shouldBe false
      Hex2048State.oneTestRow("2").shifted(North).wasChanged shouldBe false
      Hex2048State.oneTestRow("2 4 2 4 8 16 32 64").shifted(North).wasChanged shouldBe false

      Direction.All foreach { direction =>
        Hex2048State(
          Array(
            Cell(CubeCoordinate(-1, 1, 0), Filled(8)),
            Cell(CubeCoordinate(0, 1, -1), Filled(2)),
            Cell(CubeCoordinate(-1, 0, 1), Filled(2)),
            Cell(CubeCoordinate(0, 0, 0), Filled(4)),
            Cell(CubeCoordinate(1, 0, -1), Filled(8)),
            Cell(CubeCoordinate(0, -1, 1), Filled(8)),
            Cell(CubeCoordinate(1, -1, 0), Filled(2)),
          ),
        ).shifted(direction).wasChanged shouldBe false
      }
    }

  }

  "smoothed" should {
    "transform Generated, Shifted, Merged cells into Filled ones" in {
      val afterShift = Hex2048State(
        Array(
          Cell(CubeCoordinate(-2, 2, 0), Shifted(2, CubeCoordinate(-2, 0, 2))),
          Cell(CubeCoordinate(-1, 2, -1), Merged(8, CubeCoordinate(-1, 1, 0), CubeCoordinate(-1, 0, 1))),
          Cell(CubeCoordinate(0, 2, -2), Merged(4, CubeCoordinate(0, 1, -1), CubeCoordinate(0, 0, 0))),
          Cell(CubeCoordinate(-2, 1, 1), Empty),
          Cell(CubeCoordinate(-1, 1, 0), Empty),
          Cell(CubeCoordinate(0, 1, -1), Merged(4, CubeCoordinate(0, -1, 1), CubeCoordinate(0, -2, 2))),
          Cell(CubeCoordinate(1, 1, -2), Shifted(4, CubeCoordinate(1, 0, -1))),
          Cell(CubeCoordinate(-2, 0, 2), Empty),
          Cell(CubeCoordinate(-1, 0, 1), Empty),
          Cell(CubeCoordinate(0, 0, 0), Empty),
          Cell(CubeCoordinate(1, 0, -1), Shifted(2, CubeCoordinate(1, -1, 0))),
          Cell(CubeCoordinate(2, 0, -2), Empty),
          Cell(CubeCoordinate(-1, -1, 2), Empty),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
          Cell(CubeCoordinate(2, -1, -1), Empty),
          Cell(CubeCoordinate(0, -2, 2), Empty),
          Cell(CubeCoordinate(1, -2, 1), Empty),
          Cell(CubeCoordinate(2, -2, 0), Empty),
        ),
      )

      afterShift.smoothed.state.foreach {
        case Cell(_, Filled(_) | Empty) =>
        case _ => fail("Not all tiles are Empty or Filled after `smoothed` call.")
      }
    }
  }

  "withGenerated" should {
    "correctly update state with generated values" in {
      val state = Hex2048State(
        Array(
          Cell(CubeCoordinate(-1, 1, 0), Merged(8, CubeCoordinate.zeros, CubeCoordinate.zeros)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Empty),
          Cell(CubeCoordinate(0, 0, 0), Filled(4)),
          Cell(CubeCoordinate(1, 0, -1), Shifted(4, CubeCoordinate.zeros)),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
        ),
      )
      state.withGenerated(
        Array(
          Cell(CubeCoordinate(-1, 0, 1), Generated(8)),
          Cell(CubeCoordinate(0, -1, 1), Generated(4)),
        ),
      ).state shouldBe Hex2048State(
        Array(
          Cell(CubeCoordinate(-1, 1, 0), Merged(8, CubeCoordinate.zeros, CubeCoordinate.zeros)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Generated(8)),
          Cell(CubeCoordinate(0, 0, 0), Filled(4)),
          Cell(CubeCoordinate(1, 0, -1), Shifted(4, CubeCoordinate.zeros)),
          Cell(CubeCoordinate(0, -1, 1), Generated(4)),
          Cell(CubeCoordinate(1, -1, 0), Empty),
        ),
      ).state
    }
  }

  "gameStatus" should {
    "return Playing status when there is Empty tiles" in {
      val state = Hex2048State(
        Array(
          Cell(CubeCoordinate(-1, 1, 0), Filled(8)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Empty),
          Cell(CubeCoordinate(0, 0, 0), Filled(4)),
          Cell(CubeCoordinate(1, 0, -1), Filled(4)),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
        ),
      )
      state.gameStatus shouldBe GameStatus.Playing(8)
    }

    "return Playing status when there not Empty tiles, but shifting can create them" in {
      // Can be shifted on NorthEast and fours will be merged together
      val state = Hex2048State(
        Array(
          Cell(CubeCoordinate(-1, 1, 0), Filled(8)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Filled(2)),
          Cell(CubeCoordinate(0, 0, 0), Filled(4)),
          Cell(CubeCoordinate(1, 0, -1), Filled(4)),
          Cell(CubeCoordinate(0, -1, 1), Filled(8)),
          Cell(CubeCoordinate(1, -1, 0), Filled(2)),
        ),
      )
      state.gameStatus shouldBe GameStatus.Playing(8)
    }

    "return GameOver game status when there not Empty tiles and shifting changes nothing" in {
      val state = Hex2048State(
        Array(
          Cell(CubeCoordinate(-1, 1, 0), Filled(8)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Filled(2)),
          Cell(CubeCoordinate(0, 0, 0), Filled(4)),
          Cell(CubeCoordinate(1, 0, -1), Filled(8)),
          Cell(CubeCoordinate(0, -1, 1), Filled(8)),
          Cell(CubeCoordinate(1, -1, 0), Filled(2)),
        ),
      )
      state.gameStatus shouldBe GameStatus.GameOver(8)
    }
  }

  "result" should {
    "return 0 for empty state" in {
      Hex2048State.empty.result shouldBe 0
    }

    "return result for playing state" in {
      Hex2048State(
        Array(
          Cell(CubeCoordinate(-1, 1, 0), Filled(8)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Empty),
          Cell(CubeCoordinate(0, 0, 0), Filled(4)),
          Cell(CubeCoordinate(1, 0, -1), Filled(4)),
          Cell(CubeCoordinate(0, -1, 1), Empty),
          Cell(CubeCoordinate(1, -1, 0), Empty),
        ),
      ).result shouldBe 8
    }

    "return result for game over state" in {
      Hex2048State(
        Array(
          Cell(CubeCoordinate(-1, 1, 0), Filled(8)),
          Cell(CubeCoordinate(0, 1, -1), Filled(2)),
          Cell(CubeCoordinate(-1, 0, 1), Filled(2)),
          Cell(CubeCoordinate(0, 0, 0), Filled(4)),
          Cell(CubeCoordinate(1, 0, -1), Filled(8)),
          Cell(CubeCoordinate(0, -1, 1), Filled(8)),
          Cell(CubeCoordinate(1, -1, 0), Filled(2)),
        ),
      ).result shouldBe 8
    }
  }
}
