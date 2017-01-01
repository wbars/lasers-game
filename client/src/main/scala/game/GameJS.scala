package game

import config.Routes
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.html.{Div, Form, Table}
import org.scalajs.dom.raw.Element
import org.scalajs.jquery.{JQueryAjaxSettings, JQueryXHR, jQuery => $}
import upickle.default._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom._
import scalatags.JsDom.all.{id, _}
import scalatags.JsDom.tags2.section
import shared._
import upickle._

import scala.scalajs.js.Any

@JSExport
object GameJS {

  def templateBody: TypedTag[Form] = {
    form(
      button(
        onclick := { (e: Event) =>
          e.preventDefault()
          createGame()
        }, "Create game"
      )
    )
  }

  case class Coords(left: Double, top: Double, width: Double, height: Double)

  def drawLine(elem1: Element, elem2: Element, color: String) {
    def getOffset(el: Element): Coords = {
      val rect = el.getClientRects()(0)
      Coords(
        rect.left + dom.window.pageXOffset,
        rect.top + dom.window.pageYOffset,
        width = rect.width,
        height = rect.height
      )
    }

    val off1 = getOffset(elem1)
    val off2 = getOffset(elem2)
    // bottom right
    val x1 = off1.left + off1.width / 2
    val y1 = off1.top + off1.height / 2
    // top right
    val x2 = off2.left + off2.width / 2
    val y2 = off2.top + off2.height / 2
    // distance
    var length = Math.sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)))
    // center
    var cx = ((x1 + x2) / 2) - (length / 2)
    var cy = ((y1 + y2) / 2) - 1
    // angle
    var angle = Math.atan2(y1 - y2, x1 - x2) * (180 / Math.PI)
    // make hr
    val line = div(
      cls := GameGridStyleSheet.beam.name,
      style := s"" +
        s"transform: rotate(${angle}deg);" +
        s"-moz-transform: rotate(${angle}deg);" +
        s"-o-transform: rotate(${angle}deg);" +
        s"-webkit-transform: rotate(${angle}deg);" +
        s"-ms-transform: rotate(${angle}deg);" +
        s"width: ${length}px;" +
        s"top: ${cy}px;" +
        s"left: ${cx}px;" +
        s"background-color: $color"
    )
    $("body").append(line.render)
  }

  def wrapToPoints(beams: Set[(Int, Int)]): Set[Protocol.Point] = beams.map(Point.fromTuple(_))

  private def drawBeams(game: Protocol.Game) = {
    $("." + GameGridStyleSheet.beam.name).remove()
    game.beams.foreach({case (beam1, beam2, color) => drawLine(
      dom.document.getElementById(Model.getPackedPosition(beam1.tupled._1, beam1.tupled._2)),
      dom.document.getElementById(Model.getPackedPosition(beam2.tupled._1, beam2.tupled._2)),
      color
    )})
  }

  def gameBody(game: Protocol.Game): TypedTag[Div] = {
    val rows: Array[String] = game.state.split("\n")

    def tupleToPoint(coords: (Int, Int)): Protocol.Point = Protocol.Point(coords._1, coords._2)

    def updateRemoteState() = {
      $.ajax(js.Dynamic.literal(
        url = Routes.Game.updateState,
        data = write[Protocol.Move](Protocol.Move(game.id,
          tupleToPoint(Model.srcPoint), tupleToPoint(Model.targetPoint),
          wrapToPoints(Model.connections))
        ),
        success = { (_: Any, _: String, jqXHR: JQueryXHR) => renderGameGrid(jqXHR.responseText) },
        contentType = "application/json",
        `type` = "POST"
      ).asInstanceOf[JQueryAjaxSettings])
    }

    def updateGameState(e: Element) = {
      val positions = Model.getUnpackedPositions(e.id)
      if (Model.stateChanged(e)) {
        Model.changeState(positions._1, positions._2, rows(0).length, rows.length)(e)
        if (Model.isChangingStateComplete) {
          updateRemoteState()
          Model.reset()
        }
      }
    }

    def buildRow(i: Int) = {
      rows(i).indices.map(j => td(
        id := Model.getPackedPosition(i, j),
        onclick := {
          (e: Event) =>
            e.preventDefault()
            updateGameState(e.srcElement)
        },
        rows(i)(j) match {
          case '*' => ""
          case ch => ch.toString
        }
      ))
    }

    div(table(
      cls := GameGridStyleSheet.gameGrid.name,
      rows.indices.map(i => {
        tr(buildRow(i))
      })
    ))
  }

  def getGame(gameId: Int): JQueryXHR = {
    $.get(Routes.Game.gameData(gameId),
      success = {
        (_: js.Any, _: String, jqXHR: JQueryXHR) => renderGameGrid(jqXHR.responseText)
      }
    )
  }

  private def renderGameGrid(gameData: String) = {
    val content = dom.document.getElementById("content")
    content.innerHTML = ""
    val game = read[Protocol.Game](gameData)
    content.appendChild(
      section(id := "game")(
        gameBody(game)
      ).render)
    drawBeams(game)
  }

  def createGame(): JQueryXHR = {
    $.post("/game", success = {
      (_: js.Any, _: String, jqXHR: JQueryXHR) =>
        dom.window.location.assign(Routes.Game.game(jqXHR.responseText.toInt))
    })
  }


  @JSExport
  def main(): Unit = {
    $({
      addStyles()
      dom.document.getElementById("content").appendChild(
        section(id := "game")(
          templateBody
        ).render)
    })
  }

  private def addStyles() = {
    dom.document.getElementsByTagName("head")(0).appendChild(
      tags2.style(
        tpe := "text/css",
        GameGridStyleSheet.styleSheetText
      ).render
    )
  }

  @JSExport
  def gamePage(id: Int): Unit = {
    $({
      addStyles()
      getGame(id)
    })
  }
}
