package game

object ElementFactory {
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
      })
    )
    ElementFactory.createState(rows.head.length, rows.length, elems)
  }

  def fromString(stateData: String,
                 beams: Seq[((Int, Int), (Int, Int))],
                 wires: Seq[((Int, Int), (Int, Int))],
                 targets: Seq[(Int, Int)]
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
    targets.foreach(state.elements(_).asInstanceOf[Reciver].isTarget = true)

    state
  }

  def defaultState: State = fromString(
    """|****
       |****
       |A***
       |RA#r
       |BA#b""".stripMargin('|'),
    Seq.empty,
    Seq(((3, 2), (4, 3)), ((4, 2), (3, 3))),
    Seq.empty
  )
}
