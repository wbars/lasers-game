package services

import java.util
import javax.inject.Singleton

import scala.collection.mutable.Queue
import game._

import scala.collection.mutable

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


  private def isPathExists(src: (Int, Int), target: (Int, Int), state: State): Boolean = {
    val availableMoves = mutable.Queue[(Int, Int)]()
    val visitedPos = mutable.Set[(Int, Int)]()
    availableMoves.enqueue(src)

    while (availableMoves.nonEmpty) {
      val pos = availableMoves.dequeue()
      visitedPos.add(pos)

      val moves = Set(
        (pos._1 + 1, pos._2),
        (pos._1, pos._2 + 1),
        (pos._1 - 1, pos._2),
        (pos._1, pos._2 - 1)
      )
        .diff(visitedPos)
        .filter({ case (i, j) => i < state.height && i >= 0 && j < state.width && j >= 0 })
        .filter(p => !state.elements.contains(p) || (state.elements(p) match {
          case w: Wall => w.transparent
          case _ => false
        }))

      if (moves.contains(target)) return true
      moves.foreach(availableMoves.enqueue(_))
    }
    false
  }

  def moveElement(stateId: Int,
                  elementSrc: (Int, Int), elementTarget: (Int, Int),
                  connections: Set[(Int, Int)] = Set.empty): State = {
    val state = states(stateId)

    def getTargetPos: (Int, Int) =
      if (state.elements.contains(elementTarget) || !isPathExists(elementSrc, elementTarget, state)) {
        elementSrc
      } else {
        elementTarget
      }

    state.elements(elementSrc) match {
      case connector: Connector =>
        removeConcetrator(state, connector)
        val concetrator = addConcetrator(getTargetPos, connections, state)
        state.paintComponent(state.getConnectedComponent(concetrator))
        state.beamsIntersectsElement(concetrator).foreach(b => state.paintBeamElementsComponents(b))
        state
      case _ => null
    }
  }

  private def removeConcetrator(state: State, connector: Connector) {
    var intersectedBeams = connector.beams.flatMap(state.beamsIntersectsBeam)
    connector.beams.foreach(beam => state.removeBeam(beam))

    intersectedBeams = intersectedBeams ++ state.beamsIntersectsElement(connector)
    state.elements.remove((connector.x, connector.y))

    intersectedBeams.foreach(b => state.paintBeamElementsComponents(b))
  }

  private def addConcetrator(target: (Int, Int), connections: Set[(Int, Int)], state: State): Connector = {
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

    newConnector
  }
}
