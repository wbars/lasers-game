package services

import javax.inject.Singleton

import game._

@Singleton
class GameService {
  var states: Map[Int, State] = Map.empty
  var counter: Int = 0

  def createState(): Int = {
    counter += 1
    states += counter -> ElementFactory.defaultState
    counter
  }

  def state(id: Int): State = states(id)

  def updateState(id: Int,
                  src: (Int, Int), target: (Int, Int),
                  connections: Set[(Int, Int)]): State = {
    val state = states(id)
    state.elements(src) match {
      case connector: Connector =>
        removeConcetrator(state, connector)
        addConcetrator(target, connections, state)
        state
      case _ => null
    }
  }

  private def removeConcetrator(state: State, connector: Connector) {
    connector.beams.foreach(beam => state.removeBeam(beam))
    state.elements.remove((connector.x, connector.y))
  }

  private def addConcetrator(target: (Int, Int), connections: Set[(Int, Int)], state: State) = {
    def beamAddOrder(element: Element): Int = element match {
      case _: Sender => 1
      case _: Reciver => 3
      case _ => 2
    }

    val newConnector = ElementFactory.createConnector(target._1, target._2)
    state.elements += target -> newConnector
    connections.toSeq.map(state.elements(_))
      .sortWith((elem1, elem2) => beamAddOrder(elem1).compareTo(beamAddOrder(elem2)) < 0)
      .foreach({
        case c: Colored => state.addBeam(c, newConnector)
      })
  }
}
