package shared

object Protocol {
  case class Point(x: Int, y: Int) {
    def tupled: (Int, Int) = (x, y)
  }
  case class Game(id: Int, state: String, beams: Seq[(Point, Point, String)], isWin: Boolean = false)
  case class Move(id: Int, src: Point, target: Point, connections: Set[Point])
}

object Point {
  def fromTuple(tuple: (Int, Int)) = Protocol.Point(tuple._1, tuple._2)
}