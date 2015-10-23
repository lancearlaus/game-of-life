
import scala.collection.mutable
import scala.util.Random

object Main2 extends App {

  type Cell = (Int, Int)

  sealed trait Event
  case object Birth extends Event
  case object Death extends Event

  type Cells = Set[Cell]
  type Events = Map[Cell, Event]

  // Game state is the map of events to apply and the set of live cells
  case class State(cells: Cells, events: Events)

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


  // Board that defines dimensions and whether to wrap around
  case class Board(width: Int, height: Int, wrap: Boolean = false) {
    require(width > 0 && height > 0, "invalid dimension")

    def contains(cell: Cell) = cell.x >= 0 && cell.x < width && cell.y >= 0 && cell.y < height

    def constrain: PartialFunction[Cell, Cell] = {
      case c: Cell if contains(c) => c
      case (x, y) if wrap => (Math.floorMod(x, width), Math.floorMod(y, height))
    }
  }


  // Applies a set of events to adjust the set of live cells
  def applyEvents(state: State): Cells = state.events.foldLeft(mutable.Set(state.cells.toSeq: _*)) {
    case (cells, (cell, event)) => event match {
      case Birth => cells += cell
      case Death => cells -= cell
    }
  }.toSet

  // Calculates the next game state
  def nextState(state: State, board: Board): State = {

    // Apply events to get new Cell set
    val cells: Cells = applyEvents(state)

    // Derive new events based on impacted cells
    val invalidated = state.events.keySet.flatMap(_.neighbors.collect(board.constrain))
    val search = invalidated.flatMap(_.neighbors.collect(board.constrain))
    val found = search.filter(c => cells.contains(c))
    val neighborCounts = found.toSeq.flatMap(_.neighbors.collect(board.constrain))
      .groupBy(identity)
      .map { case (cell, neighbors) => (cell -> neighbors.size) }
    // Catch cells that have no neighbors
    val zeroCounts = Map(found.toSeq.filterNot(c => neighborCounts.contains(c)).map(c => (c -> 0)): _*)

    val events: Events = (neighborCounts ++ zeroCounts)
      .map { case (cell, count) => (cell -> (cells.contains(cell), count)) }
      .collect {
        case (cell, (false, 3))                    => (cell -> Birth)
        case (cell, (true, n)) if (n < 2 || n > 3) => (cell -> Death)
      }

    State(cells, events)
  }

  // Generate random board state with the given live cell density
  def randomState(board: Board, density: Double = 0.2): State = {
    require(density > 0 && density <= 1, "invalid density")

    val count = (board.width * board.height * density).toInt
    val events = (1 to count).foldLeft(mutable.Map[Cell, Event]()) { case (events, i) =>
      events += (((Random.nextInt(board.width), Random.nextInt(board.height))) -> Birth)
    }.toMap

    State(Set.empty[Cell], events)
  }

  def printState(state: State, board: Board, live: Char = 'X') = {
    val margin = board.height.toString.length
    val groupedEvents = state.events.groupBy { case (k, v) => v }.map { case (event, events) => (event -> events.size) }
    println(s"   Events: $groupedEvents")
    println(s"   Cells : ${state.cells.size}")
    println("   ".padTo(board.width, '-'))

    val formatstr = s"%${margin}d"
    for (y <- 0 until board.height) {
      print(y.formatted(formatstr) + ":")
      for (x <- 0 until board.width) {
        val c = if (state.cells.contains((x, y))) live else ' '
        print(c)
      }
      println()
    }
  }

  // Read state from a multi-line string (see tests for examples)
  def readState(s: String): State = {
    val events = mutable.Map[Cell, Event]()
    s.foldLeft((0,0)) { case ((x, y), c) =>
      c match {
        case '\n' => (0, y + 1)
        case ' '  => (x + 1, y)
        case _    => {
          events += ((x, y) -> Birth)
          (x + 1, y)
        }
      }
    }
    State(Set.empty[Cell], events.toMap)
  }


  val board = Board(80, 80, true)
  val iterations = 200

  (1 to iterations).foldLeft(randomState(board)) { case (state, i) =>
    println("   " + "".padTo(board.width, '='))
    printState(state, board)
    nextState(state, board)
  }
}
