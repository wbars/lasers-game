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

  test("Wires should toggle walls") {
    val state = ElementFactory.fromString(
      """|AA****#
         |R#***#r
         |******#
         |B*****b""".stripMargin('|'),
      Seq(((1, 0), (0, 1))),
      Seq(((0, 6), (3, 6)), ((2, 6), (3, 6)), ((1, 5), (3, 6)), ((1, 1), (3, 6))),
      Seq((1, 6))
    )
    gameService.states += 1 -> state

    gameService.moveElement(1, (0, 1), (1, 3), Set((1, 0), (1, 6)))
    state.beams should have size 2
    all(state.beams) should have('color (Absent))
    state should have('isWinState (false))

    gameService.moveElement(1, (0, 0), (3, 3), Set((3, 0), (3, 6)))
    gameService.moveElement(1, (1, 3), (1, 3), Set((1, 0), (1, 6)))
    state.beams should have size 4
    all(state.beams) should not have 'color (Absent)
    state should have('isWinState (true))
  }

  test("Connnector should not replace transparent wall") {
    val state = ElementFactory.fromString(
      """|A*#
         |RAr""".stripMargin,
      Seq(((1, 2), (1, 1)), ((1, 0), (1, 1))),
      Seq(((0, 2), (1, 2))),
      Seq((1, 2)
      )
    )
    gameService.states += 1 -> state
    gameService.state(1).elements((0, 2)).asInstanceOf[Wall] should have('transparent (true))
    gameService.moveElement(1, (0, 0), (0, 2), Set.empty)

    gameService.state(1).elements((0, 2)) shouldBe a[Wall]
    gameService.state(1).elements((0, 0)) shouldBe a[Connector]
  }

  test("Deadlock problem should be solved") {
    val state = ElementFactory.fromString(
      """|*A**
         |****
         |****
         |RA#r
         |BA#b""".stripMargin('|'),
      Seq.empty,
      Seq(((3, 2), (4, 3)), ((4, 2), (3, 3))),
      Seq((3, 3), (4, 3))
    )
    gameService.states += 1 -> state
    gameService.moveElement(1, (0, 1), (0, 1), Set((3, 0), (3, 3)))
    all(state.beams) should have('color (Red))
    state should have('isWinState (false))

    gameService.moveElement(1, (4, 1), (4, 1), Set((4, 0), (4, 3)))
    gameService.moveElement(1, (3, 1), (3, 1), Set((3, 0), (3, 3)))
    all(state.beams) should not have 'color (Absent)
    state should have('isWinState (true))

    gameService.moveElement(1, (0, 1), (0, 1), Set.empty)
    all(state.beams) should not have 'color (Absent)
    state should have('isWinState (true))

    gameService.moveElement(1, (3, 1), (2, 1), Set.empty)
    state should have('isWinState (false))
  }

  test("Release beam by removing intersecting concetrator should propagate on connected recievers") {
    val state = ElementFactory.fromString(
      """|*A**
         |****
         |****
         |RA#r
         |BA#b""".stripMargin('|'),
      Seq.empty,
      Seq(((3, 2), (4, 3)), ((4, 2), (3, 3))),
      Seq((3, 3), (4, 3))
    )
    gameService.states += 1 -> state
    gameService.moveElement(1, (0, 1), (0, 1), Set((3, 0), (3, 3)))
    gameService.moveElement(1, (4, 1), (4, 1), Set((4, 0), (4, 3)))
    assert(gameService.state(1).elements((4, 3)).asInstanceOf[Reciver].isActive)

    gameService.moveElement(1, (3, 1), (2, 1), Set.empty)
    assert(!gameService.state(1).elements((4, 3)).asInstanceOf[Reciver].isActive)

    gameService.moveElement(1, (2, 1), (3, 1), Set.empty)
    assert(gameService.state(1).elements((4, 3)).asInstanceOf[Reciver].isActive)
  }

  test("Simple move can be achieved") {
    val state = ElementFactory.fromString(
      """|A*****
         |******
         |******""".stripMargin)
    gameService.states += 1 -> state
    gameService.moveElement(1, (0, 0), (2, 5))
    gameService.state(1).elements(2, 5) shouldBe a[Connector]
  }

  test("Obstacles should block moving") {
    val state = ElementFactory.fromString(
      """|**#**
         |*#A#*
         |**#**""".stripMargin)
    gameService.states += 1 -> state
    gameService.moveElement(1, (1, 2), (0, 0))
    gameService.state(1).elements(1, 2) shouldBe a[Connector]

    val state2 = ElementFactory.fromString(
      """|**A**
         |*AAA*
         |**A**""".stripMargin)
    gameService.states += 1 -> state2
    gameService.moveElement(1, (1, 2), (0, 0))
    gameService.state(1).elements(1, 2) shouldBe a[Connector]

    val state3 = ElementFactory.fromString(
      """|**r**
         |*BAR*
         |**b**""".stripMargin)
    gameService.states += 1 -> state3
    gameService.moveElement(1, (1, 2), (0, 0))
    gameService.state(1).elements(1, 2) shouldBe a[Connector]

    gameService.moveElement(1, (1, 2), (0, 4))
    gameService.state(1).elements(1, 2) shouldBe a[Connector]

    gameService.moveElement(1, (1, 2), (2, 4))
    gameService.state(1).elements(1, 2) shouldBe a[Connector]

    gameService.moveElement(1, (1, 2), (2, 0))
    gameService.state(1).elements(1, 2) shouldBe a[Connector]
  }

  test("Transparent walls should allow moving") {
    val state = ElementFactory.fromString(
      """|A#***R
         |*#***A
         |*#***r""".stripMargin,
      Seq.empty,
      Seq(((0, 1), (2, 5)), ((1, 1), (2, 5)), ((2, 1), (2, 5))),
      Seq.empty
    )
    gameService.states += 1 -> state
    gameService.moveElement(1, (0, 0), (0, 2))
    gameService.state(1).elements(0, 0) shouldBe a[Connector]

    gameService.moveElement(1, (1, 5), (1, 5), Set((0, 5), (2, 5)))
    gameService.moveElement(1, (0, 0), (0, 2))
    gameService.state(1).elements(0, 2) shouldBe a[Connector]
  }
}
