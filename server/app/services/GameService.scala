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

  def removeJammer(state: State, jammer: Jammer) {
    state.elements.remove((jammer.x, jammer.y))
    jammer.target match {
      case Some(w: Wall) => w.jammers -= jammer
      case _ =>
    }
  }

  def addJammer(position: (Int, Int), target: Option[(Int, Int)], state: State): Jammer = {
    val jammer = ElementFactory.createJammer(position._1, position._2)
    target match {
      case Some(pos) => state.elements(pos) match {
        case w: Wall =>
          w.jammers += jammer
          jammer.target = Some(w)
        case _ =>
      }
      case _ =>
    }
    state.elements += position -> jammer
    jammer
  }

  def refreshJammer(jammer: Jammer, state: State) {
    jammer.active = jammer.target match {
      case Some(w: Wall) => !state.isLineIntersectWall(w, jammer)
      case _ => false
    }
  }

  def getBlockingJammers(jammer: Jammer, state: State): Set[Jammer] = jammer.target match {
    case Some(wall: Wall) => state.jammers.filter(j => j.target match {
      case Some(w: Wall) => State.isLineIntersectsElement(new Line(j, w), wall)
      case _ => false
    }).toSet
    case _ => Set.empty
  }

  def refreshJammersWithPropagating(jammers: mutable.Iterable[Jammer], state: State) {
    val affectedJammers = mutable.Queue[Jammer]()
    val visitedJammers = mutable.Set[Jammer]()
    jammers.foreach(affectedJammers.enqueue(_))
    visitedJammers ++= jammers

    while (affectedJammers.nonEmpty) {
      val jammer = affectedJammers.dequeue()
      val currentAffectedJammers: Set[Jammer] = getBlockingJammers(jammer, state)
      refreshJammer(jammer, state)
      currentAffectedJammers.filter(!visitedJammers.contains(_))
        .foreach(j => {
          refreshJammer(j, state)
          visitedJammers += j
        })
    }
  }

  def moveElement(stateId: Int,
                  elementSrc: (Int, Int), elementTarget: (Int, Int),
                  connections: Set[(Int, Int)] = Set.empty): State = {
    val state = states(stateId)

    def targetPos: (Int, Int) =
      if (state.elements.contains(elementTarget) || !isPathExists(elementSrc, elementTarget, state)) {
        elementSrc
      } else {
        elementTarget
      }

    def moveConnector(connector: Connector) = {
      removeConnector(state, connector)
      val newConnector = addConnector(targetPos, connections, state)
      state.paintComponent(state.getConnectedComponent(newConnector))
      state.beamsIntersectsElement(newConnector).foreach(b => state.paintBeamElementsComponents(b))
      state
    }

    def moveJammer(jammer: Jammer): State = {
      def getJammerAffectedBeams(j: Jammer): mutable.Set[Beam] = j.target match {
        case Some(w: Wall) => state.beamsIntersectsElement(w)
        case _ => mutable.Set.empty
      }

      val intersectingBeams: mutable.Set[Beam] = getJammerAffectedBeams(jammer)
      val affectedJammers: mutable.Set[Jammer] = mutable.Set(getBlockingJammers(jammer, state).toList:_*)

      removeJammer(state, jammer)

      def target = connections.headOption match {
        case Some(pos) => state.elements.get(pos) match {
          case Some(_: Wall) => Some(pos)
          case _ => None
        }
        case _ => None
      }

      val newJammer: Jammer = addJammer(targetPos, target, state)
      refreshJammer(newJammer, state)

      (intersectingBeams ++ getJammerAffectedBeams(newJammer)).foreach(state.paintBeamElementsComponents(_))
      refreshJammersWithPropagating(affectedJammers ++ getBlockingJammers(newJammer, state), state)
      state
    }

    state.elements(elementSrc) match {
      case connector: Connector => moveConnector(connector)
      case jammer: Jammer => moveJammer(jammer)
      case _ => null
    }
  }

  private def removeConnector(state: State, connector: Connector) {
    var intersectedBeams = connector.beams.flatMap(state.beamsIntersectsBeam)
    connector.beams.foreach(beam => state.removeBeam(beam))

    intersectedBeams = intersectedBeams ++ state.beamsIntersectsElement(connector)
    state.elements.remove((connector.x, connector.y))

    intersectedBeams.foreach(b => state.paintBeamElementsComponents(b))
  }

  private def addConnector(target: (Int, Int), connections: Set[(Int, Int)], state: State): Connector = {
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
