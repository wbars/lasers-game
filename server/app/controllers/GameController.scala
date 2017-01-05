package controllers

import javax.inject.Inject

import game.{Beam, ElementFactory, State}
import services.GameService
import shared._
import upickle.default._
import play.api.mvc.{Action, Controller}


class GameController @Inject() (gameService: GameService) extends Controller {

  def index() = Action { implicit request =>
    Ok(views.html.game_index(request))
  }

  def gamePage(id: Int) = Action { implicit request =>
    Ok(views.html.game_page(id))
  }

  def createGame() = Action {
    Ok(gameService.createState().toString)
  }

  def gameData(id: Int) = Action {
    val state = gameService.state(id)
    Ok(write[Protocol.Game](wrapToGame(id, state)))
  }

  def updateState() = Action { implicit request =>
    val move: Protocol.Move = read[Protocol.Move](request.body.asJson.get.toString())
    Ok(write[Protocol.Game](wrapToGame(move.id, gameService.moveElement(
      move.id,
      move.src.tupled, move.target.tupled,
      move.connections.map(_.tupled))
    ))
    )
  }

  private def beamsToPoints(beams: Seq[Beam]): Seq[(Protocol.Point, Protocol.Point, String)] =
    beams.map(beam => (Protocol.Point(beam.colored.x, beam.colored.y), Protocol.Point(beam.connector.x, beam.connector.y), beam.color.toString))

  private def wrapToGame(id: Int, state: State): Protocol.Game = Protocol.Game(id, state.toString, beamsToPoints(state.beams.toSeq))
}
