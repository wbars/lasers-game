package game

import config.Routes
import org.scalajs.dom
import org.scalajs.dom.raw.Element
import org.scalajs.jquery.{JQueryXHR, jQuery => $}
import shared._
import upickle._
import upickle.default._

import scala.concurrent.Future
import scala.scalajs.js
import scalatags.JsDom.all.{id, _}
import scalatags.JsDom.tags2.section

trait SelectedElement

case class Connector() extends SelectedElement

case class Jammer() extends SelectedElement

object Model {
  var connections: Set[(Int, Int)] = Set.empty
  var srcPoint: (Int, Int) = (0, 0)
  var targetPoint: (Int, Int) = (0, 0)
  var selectedElement: Option[SelectedElement] = None

  def stateChanged(selectedElem: Element): Boolean = grabbed != null || (grabbed == null && (isConcetrator(selectedElem) || isJammer(selectedElem)))

  var itemGrabbed: Boolean = false
  var grabbed: Element = _

  def isConcetrator(elem: Element): Boolean = elem.textContent == "A"

  def isJammer(elem: Element): Boolean = elem.textContent == "J"

  def getPackedPosition(i: Int, j: Int): String = s"${i}_$j"

  def getUnpackedPositions(elemId: String): (Int, Int) = elemId.split("_") match {
    case Array(i: String, j: String) => (i.toInt, j.toInt)
  }

  def changeState(i: Int, j: Int, width: Int, height: Int, elem: Element) {
    changeTable(elem)
    getStateFromTable(width, height)
  }

  def isChangingStateComplete: Boolean = grabbed == null

  def reset() {
    grabbed = null
    connections = Set.empty
  }

  private def changeTable(elem: Element) = {
    val $elem = $(elem)

    def toggleConnection(connection: (Int, Int)) = {
      if (connections.contains(connection)) {
        connections -= connection
        $elem.removeClass(GameGridStyleSheet.selected.name)
      } else {
        connections += connection
        $elem.addClass(GameGridStyleSheet.selected.name)
      }
    }

    def moveElement(elemPos: (Int, Int)) = {
      targetPoint = elemPos
      $(grabbed).removeClass(GameGridStyleSheet.active.name)
      grabbed = null
      selectedElement = None
    }

    def grabElement() {
      srcPoint = getUnpackedPositions(elem.id)
      $elem.addClass(GameGridStyleSheet.active.name)
      grabbed = elem
    }

    def moveConnector(pos: (Int, Int)) = elem.textContent match {
      case "" => moveElement(pos)
      case "A" if elem == grabbed => moveElement(pos)
      case "R" | "r" | "B" | "b" | "A" => toggleConnection(pos)
    }

    def moveJammer(pos: (Int, Int)) = if ($elem.hasClass(GameGridStyleSheet.wall.name)) {
      connections = Set.empty
      if ($elem.hasClass(GameGridStyleSheet.active.name)) {
        $elem.removeClass(GameGridStyleSheet.active.name)
      } else {
        $(s".${GameGridStyleSheet.active.name}.${GameGridStyleSheet.wall.name}").removeClass(GameGridStyleSheet.active.name)
        $elem.addClass(GameGridStyleSheet.active.name)
        connections += pos
      }

    } else {
      elem.textContent match {
        case "" => moveElement(pos)
        case "J" if elem == grabbed => moveElement(pos)
      }
    }


    if (grabbed == null) {
      if (isConcetrator(elem)) {
        selectedElement = Some(Connector())
      } else if (isJammer(elem)) {
        selectedElement = Some(Jammer())
      }

      selectedElement match {
        case Some(_) => grabElement()
      }
    } else if (grabbed != null) {
      val elemPos = getUnpackedPositions(elem.id)
      selectedElement match {
        case Some(_: Connector) => moveConnector(elemPos)
        case Some(_: Jammer) => moveJammer(elemPos)
        case _ =>
      }
    }
  }

  def getStateFromTable(width: Int, height: Int): String = {
    def buildRow(i: Int) = {
      Range(0, width).map(j => $(s"#${getPackedPosition(i, j)}").text match {
        case "" => "*"
        case text => text
      }).mkString
    }

    Range(0, height).map(buildRow).mkString("\n")
  }
}