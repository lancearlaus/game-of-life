import Main._
import org.scalatest.{Matchers, WordSpec}

class MainSpec extends WordSpec with Matchers {

  "Point neighbors" should {
    "produce adjacent cells" in {

      import CellOps._

      val expected = Set(
        (0, 0), (0, 1), (0, 2),
        (1, 0),         (1,2),
        (2, 0), (2, 1), (2, 2))
      val actual = (1,1).neighbors.toSet

      actual shouldBe expected
    }
  }

  "Bounds" should {

    "contain valid points" in {
      val bounds = Bounds(10, 10)
      val valid = Seq( (0,0), (0,9), (9,0), (9,9))

      valid.foreach(p => bounds.contains(p) shouldBe true)
    }

    "not contain invalid points" in {
      val bounds = Bounds(10, 10)
      val valid = Seq( (-1,0), (0,-1), (10,0), (0,10), (10,10))

      valid.foreach(p => bounds.contains(p) shouldBe false)
    }

    "constrain valid points unmodified" in {
      val bounds = Bounds(10, 10)
      val valid = Seq( (0,0), (0,9), (9,0), (9,9))

      valid.collect(bounds.constrain) shouldBe valid
    }

    "filter invalid points in unwrapped contrain" in {
      val bounds = Bounds(10, 10)
      val valid = Seq( (-1,0), (0,-1), (10,0), (0,10), (10,10))

      valid.collect(bounds.constrain) shouldBe empty
    }

    "wrap invalid points in wrapped contrain" in {
      val bounds = Bounds(10, 10, true)
      val valid = Seq( (-1,0), (0,-1), (10,0), (0,10), (10,10))
      val expected = Seq( (9,0), (0,9), (0,0), (0,0), (0,0))

      valid.collect(bounds.constrain) shouldBe expected
    }

  }

  "readState" should {
    "read string state" in {

      val bounds = Bounds(10, 10)
      val state =
        """
          |
          | XXXXXXXX
          | XXXXXXXX
          | XXXXXXXX
          | XXXXXXXX
          | XXXXXXXX
          | XXXXXXXX
          | XXXXXXXX
          | XXXXXXXX
        """.stripMargin.stripPrefix("\n").stripSuffix("\n")

      val expected =(
        for {
          x <- 1 until bounds.width - 1
          y <- 1 until bounds.height - 1
        } yield (x, y)).toSet

      val actual = readState(state)

      actual shouldBe expected
    }
  }

  "nextState" should {

    "produce correct next state" in {
      val bounds = Bounds(10, 10)
      
      val initialState =
        """
          |
          | XXXXXXXX
          | XXXXXXXX 
          | XXXXXXXX 
          | XXXXXXXX 
          | XXXXXXXX 
          | XXXXXXXX 
          | XXXXXXXX 
          | XXXXXXXX 
        """.stripMargin.stripPrefix("\n").stripSuffix("\n")
      
      val expectedState =
        """
          |  XXXXXX  
          | X      X 
          |X        X
          |X        X
          |X        X
          |X        X
          |X        X
          |X        X
          | X      X 
          |  XXXXXX  
        """.stripMargin.stripPrefix("\n").stripSuffix("\n")
      
      val initial = readState(initialState)
      val expected = readState(expectedState)
      val actual = nextState(initial, bounds)

      actual shouldBe expected
    }
  }
}
