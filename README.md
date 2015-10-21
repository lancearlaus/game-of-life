# game-of-life

Learning / Teaching project implementing Conway's Game of Life.

Demonstrates the basics of functional programming in Scala, the use of collection operations, and a few features of the
language, including

* [Type Alias](http://www.scala-lang.org/files/archive/spec/2.11/04-basic-declarations-and-definitions.html#type-declarations-and-type-aliases)
* [Implicit Classes](http://docs.scala-lang.org/overviews/core/implicit-classes.html)
* [Partial Functions](http://www.scala-lang.org/api/current/index.html#scala.PartialFunction)
 
## Notes

The implementation is stateless with each iteration taking the current state (set of live cells) as input,
and generating the next state. Though simple, this requires traversing the entire state, even when no changes have
occurred.

A more efficient, but complex, implementation may instead define game state as the current set of live cells plus the
set of events (birth/death) produced by the previous round. The state transition function can then optimize its
calculation the impacted area. For example,

````scala
type Cell = (Int, Int)
type Cells = Set[Cell]

sealed trait Event
case object Birth extends Event
case object Death extends Event

type Events = Map[Cell, Event]

case class State(cells: Cells, events: Events)

def nextState(state: State, bounds: Bounds): State = {
    // Calculate next state and resulting events
}
````