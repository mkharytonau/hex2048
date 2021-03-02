package hex2048

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import Hex2048.Cell

class Hex2048Spec extends AnyWordSpec with Matchers {
  import Hex2048.Tile._

  val hex2048 = Hex2048.empty

  "parseString" should {
    "not fail on empty string" in {
      hex2048.parseString("") shouldBe Array.empty
    }

    "parse one element string" in {
      hex2048.parseString("1").map(_.tile) shouldBe Array(Filled(1))
    }

    "recognize '_' as Empty tile" in {
      hex2048.parseString("_").map(_.tile) shouldBe Array(Empty)
    }

    "take into account just powers of 2" in {
      hex2048.parseString("1 2 3 4 5").map(_.tile) shouldBe Array(
        Filled(1),
        Filled(2),
        Filled(4),
      )
    }

    "parse '_' along with numbers" in {
      hex2048.parseString("_ 2 _ 4 _ _ 8").map(_.tile) shouldBe Array(
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

  "shift" should {
    "correctly shift empty list" in {
      hex2048.shift(Array.empty) shouldBe Array.empty
    }

    "correctly shift '2'" in {
      hex2048.shift(hex2048.parseString("2")).map(_.tile) shouldBe Array(Filled(2))
    }

    "correctly shift '_ 2'" in {
      hex2048.shift(hex2048.parseString("_ 2")).map(_.tile) shouldBe Array(Empty, Filled(2))
    }

    "correctly shift '2 2'" in {
      hex2048.shift(hex2048.parseString("2 2")).map(_.tile) shouldBe Array(
        Empty,
        Merged(4, 1, 0),
      )
    }

    "correctly shift '2 2 2'" in {
      hex2048.shift(hex2048.parseString("2 2 2")).map(_.tile) shouldBe Array(
        Empty,
        Shifted(2, 0),
        Merged(4, 2, 1),
      )
    }

    "correctly shift '2 2 2 _'" in {
      hex2048.shift(hex2048.parseString("2 2 2 _")).map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Shifted(2, 0),
        Merged(4, 2, 1),
      )
    }

    "correctly shift '_ 2 2 _'" in {
      hex2048.shift(hex2048.parseString("_ 2 2 _")).map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Empty,
        Merged(4, 2, 1),
      )
    }

    "correctly shift '_ _ _ _'" in {
      hex2048.shift(hex2048.parseString("_ _ _ _")).map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Empty,
        Empty,
      )
    }

    "correctly shift '16 _ _ 16 2 8 4 2 2 _'" in {
      hex2048.shift(hex2048.parseString("16 _ _ 16 2 8 4 2 2 _")).map(_.tile) shouldBe Array(
        Empty,
        Empty,
        Empty,
        Empty,
        Empty,
        Merged(32, 3, 0),
        Shifted(2, 4),
        Shifted(8, 5),
        Shifted(4, 6),
        Merged(4, 8, 7),
      )
    }
  }
}
