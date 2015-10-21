
import collection.mutable
import util.Random

object Main extends App {

  type Cell = (Int, Int)

  // Adds convenience methods to generate neighbors and to interpret standard Tuple as xy coordinates
  object CellOps {
    implicit class Neighbors(val cell: Cell) extends AnyVal {
      def x = cell._1
      def y = cell._2
      def neighbors = for {
        nx <- x - 1 to x + 1
        ny <- y - 1 to y + 1
        if (nx != x || ny != y)
      } yield (nx, ny)
    }
  }

  import CellOps._

  case class Bounds(width: Int, height: Int, wrap: Boolean = false) {
    require(width > 0 && height > 0, "invalid dimension")

    def contains(cell: Cell) = cell.x >= 0 && cell.x < width && cell.y >= 0 && cell.y < height

    def constrain: PartialFunction[Cell, Cell] = {
      case c: Cell if contains(c) => c
      case (x, y) if wrap => (Math.floorMod(x, width), Math.floorMod(y, height))
    }
  }

  // Game state is the set of live cells
  type State = Set[Cell]

  // Calculates the next game state
  def nextState(state: State, bounds: Bounds): State = {
    state.toSeq.flatMap(_.neighbors.collect(bounds.constrain)).groupBy(identity)
      .filter { case (cell, neighbors) =>
        (state.contains(cell), neighbors.size) match {
          case (true, 2 | 3) => true    // Survival
          case (false, 3)    => true    // Birth
          case _             => false
        }
    }.keySet
  }

  // Generate random board state with the given live cell density
  def randomState(bounds: Bounds, density: Double = 0.2) = {
    require(density > 0 && density <= 1, "invalid density")

    val count = (bounds.width * bounds.height * density).toInt

    (1 to count).foldLeft(mutable.Set[Cell]()) { case (state, i) =>
      state += ((Random.nextInt(bounds.width), Random.nextInt(bounds.height)))
    }.toSet
  }

  def printState(state: State, bounds: Bounds, live: Char = '#') = {
    for (x <- 0 until bounds.width) {
      for (y <- 0 until bounds.height) {
        val c = if (state.contains((x, y))) live else ' '
        print(c)
      }
      println()
    }
  }

  // Read state from a multi-line string (see tests for examples)
  def readState(s: String): State = {
    val state = mutable.Set[Cell]()
    s.foldLeft((0,0)) { case ((x, y), c) =>
      c match {
        case '\n' => (0, y + 1)
        case ' '  => (x + 1, y)
        case _    => {
          state += ((x, y))
          (x + 1, y)
        }
      }
    }
    state.toSet
  }


  val bounds = Bounds(80, 80, true)
  val iterations = 1000

  (1 to iterations).foldLeft(randomState(bounds)) { case (state, i) =>
    println("".padTo(bounds.width, '='))
    printState(state, bounds)
    nextState(state, bounds)
  }
}
