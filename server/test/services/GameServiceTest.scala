package services

import game.ElementFactory.{createConnector, createSender, createState}
import game._
import services.GameService
import org.scalatest.{FunSuite, Matchers}

class GameServiceTest extends FunSuite with Matchers {
  val gameService = new GameService()

  test("Moving element should affect intesecting beams color") {
    val sender = createSender(0, 0, Red)
    val connector = createConnector(0, 2)
    val connector1 = createConnector(2, 2)
    val state = createState(3, 3, Seq(sender, connector, connector1))
    state.addBeam(sender, connector)
    gameService.states += 1 -> state

    state.beams.head should have('color (Red))
    gameService.moveElement(1, (2, 2), (0, 1), Set.empty)
    state.beams.head should have('color (Absent))

    gameService.moveElement(1, (0, 1), (2, 2), Set.empty)
    state.beams.head should have('color (Red))
  }
}
