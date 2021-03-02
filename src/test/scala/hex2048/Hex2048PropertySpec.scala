package hex2048

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen
import hex2048.Hex2048.Cell
import hex2048.Hex2048.Tile
import hex2048.Hex2048.Tile._
import org.scalacheck.Test.Parameters

class Hex2048PropertySpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  implicit val params = Parameters.default.withMinSuccessfulTests(1000)

  val hex2048 = Hex2048.empty

  val tileValue = Gen.choose(1, 12).map(e => Integer.parseInt("1" + "0" * e, 2))

  val tile = Gen.frequency(
    1 -> Cell.justTile(Empty),
    1 -> tileValue.map(Filled).map(Cell.justTile),
  )

  val tilesList = for {
    n <- Gen.choose(0, 20)
    list <- Gen.listOfN(n, tile)
  } yield list

  "tiles list" should "stays the same length after shifting" in {
    forAll(tilesList) { tiles =>
      hex2048.shift(tiles.toArray).length shouldBe (tiles.length)
    }
  }

  "shifted tiles list" should "not contain two tiles with the same values nearby" +
    "(except at least one of them was already merged)" in {
      forAll(tilesList) { tiles =>
        val shifted = hex2048.shift(tiles.toArray).map(_.tile)
        (shifted.zip(shifted.drop(1)) collect { case (c1: HasValue, c2: HasValue) => c1 -> c2 } collect {
          case (c1 @ (Filled(_) | Shifted(_, _)), c2 @ (Filled(_) | Shifted(_, _))) if c1.value == c2.value =>
        }) shouldBe empty
      }
    }

  "shifted tiles list" should "have all Empty tiles floated to the start" in {
    forAll(tilesList) { tiles =>
      whenever(tiles.map(_.tile).exists {
        case Empty => false
        case _ => true
      }) {
        val shifted = hex2048.shift(tiles.toArray).map(_.tile)
        val indexOfFirstNonEmpty = shifted.indexWhere {
          case Empty => false
          case _ => true
        }
        shifted.drop(indexOfFirstNonEmpty + 1) collect { case Empty => } shouldBe empty
      }
    }
  }

  "shifted tiles list" should "have Merged tiles" in {
    forAll(tilesList) { tiles =>
      val nonEmpties = tiles.map(_.tile).collect { case t: Filled =>
        t
      }
      val cond = nonEmpties.zip(nonEmpties.drop(1)) collect {
        case (t1, t2) if t1.value == t2.value =>
      }
      whenever(cond.size > 0) {
        val shifted = hex2048.shift(tiles.toArray).map(_.tile)
        shifted.exists(t => {
          t match {
            case _: Merged => true
            case _ => false
          }
        }) shouldBe true
      }
    }
  }

  "from, parent1, parent2 indexes" should "should be in range [0; tiles.length) for shifted tiles" in {
    forAll(tilesList) { tiles =>
      val shifted = hex2048.shift(tiles.toArray).map(_.tile)
      shifted.exists(t => {
        t match {
          case Shifted(_, from) => from < 0 || from >= tiles.length
          case Merged(_, parent1, parent2) =>
            parent1 < 0 || parent1 >= tiles.length || parent2 < 0 || parent2 >= tiles.length
          case _ => false
        }
      }) shouldBe false
    }
  }
}
