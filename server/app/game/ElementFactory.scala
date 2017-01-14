package game

import scala.collection.mutable

object ElementFactory {
  def createJammer(i: Int, j: Int): Jammer = Jammer(i, j)()

  def createWall(x: Int, y: Int): Wall = Wall(x, y)

  def createSender(x: Int, y: Int, color: Color): Sender = Sender(x, y, color)()

  def createReciver(x: Int, y: Int, color: Color, isTarget: Boolean = false): Reciver = Reciver(x, y, color, isTarget)()

  def createConnector(x: Int, y: Int): Connector = Connector(x, y)()

  def createState(width: Int, height: Int, elems: Seq[Element]) = new State(width, height, collection.mutable.Map() ++ elems.map(e => (e.x, e.y) -> e).toMap)

  def fromString(state: String): State = {
    val rows: Array[String] = state.split("\n")
    val elems = rows.indices.flatMap(i => Range(0, rows(i).length)
      .filter(j => rows(i)(j) != '*')
      .map(j => rows(i)(j) match {
        case 'R' => ElementFactory.createSender(i, j, Red)
        case 'B' => ElementFactory.createSender(i, j, Blue)
        case 'r' => ElementFactory.createReciver(i, j, Red)
        case 'b' => ElementFactory.createReciver(i, j, Blue)
        case '#' => ElementFactory.createWall(i, j)
        case 'A' => ElementFactory.createConnector(i, j)
        case 'J' => ElementFactory.createJammer(i, j)
      })
    )
    ElementFactory.createState(rows.head.length, rows.length, elems)
  }

  def fromString(stateData: String,
                 beams: Map[(Int, Int), (Int, Int)],
                 wires: Map[(Int, Int), (Int, Int)],
                 targets: Iterable[(Int, Int)],
                 jammers: Map[(Int, Int), (Int, Int)] = Map.empty
                ): State = {
    val state = fromString(stateData)
    beams.foreach(t => state.addBeam(
      state.elements(t._1).asInstanceOf[Colored],
      state.elements(t._2).asInstanceOf[Connector]
    ))

    wires.foreach(t => {
      state.elements(t._1).asInstanceOf[Wall].wires += state.elements(t._2).asInstanceOf[Reciver]
      state.elements(t._2).asInstanceOf[Reciver].wires += state.elements(t._1).asInstanceOf[Wall]
    })

    state.jammers.foreach(_.target = None)
    state.walls.foreach(_.jammers.clear())
    jammers.foreach({case (jammer, target) => state.elements(jammer) match {
      case j: Jammer => state.elements(target) match {
        case w: Wall =>
          j.target = Some(w)
          w.jammers += j
        case _ =>
      }
      case _ =>
    }})

    targets.foreach(state.elements(_).asInstanceOf[Reciver].isTarget = true)

    state
  }

  def defaultState: State = fromString(
    """|****
       |****
       |A***
       |RA#r
       |BA#b""".stripMargin('|'),
    Map.empty,
    Map((3, 2) -> (4, 3), (4, 2) -> (3, 3)),
    Seq((3, 3), (4, 3))
  )
}
