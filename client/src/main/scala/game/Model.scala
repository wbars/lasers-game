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

object Model {
  var connections: Set[(Int, Int)] = Set.empty
  var srcPoint: (Int, Int) = (0, 0)
  var targetPoint: (Int, Int) = (0, 0)

  def stateChanged(selectedElem: Element): Boolean = grabbed != null || (grabbed == null && isConcetrator(selectedElem))

  var itemGrabbed: Boolean = false
  var grabbed: Element = _

  def isConcetrator(elem: Element): Boolean = elem.textContent == "A"

  def getPackedPosition(i: Int, j: Int): String = s"${i}_$j"

  def getUnpackedPositions(elemId: String): (Int, Int) = elemId.split("_") match {
    case Array(i: String, j: String) => (i.toInt, j.toInt)
  }

  def changeState(i: Int, j: Int, width: Int, height: Int): (Element) => String = (elem: Element) => {
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
      dom.console.log("Toggled")
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
    }

    def grabElement() {
      srcPoint = getUnpackedPositions(elem.id)
      $elem.addClass(GameGridStyleSheet.active.name)
      grabbed = elem
    }

    dom.console.log(elem.textContent)
    if (grabbed == null && isConcetrator(elem)) {
      grabElement()
    } else if (grabbed != null) {
      val elemPos = getUnpackedPositions(elem.id)
      dom.console.log(elem.textContent)
      elem.textContent match {
        case "" => moveElement(elemPos)
        case "A" if elem == grabbed => moveElement(elemPos)
        case "R" | "r" | "B" | "b" | "A" => toggleConnection(elemPos)
      }
    } else {
      dom.console.log("Else")
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