package game

import game.ElementFactory._
import org.scalatest.{FunSuite, Matchers}
import services.GameService

class StateTest extends FunSuite with Matchers {
  test("Empty state should consist only of dots") {
    val emptyState = createState(5, 4, Seq.empty)
    assert(emptyState.toString ==
      """|*****
         |*****
         |*****
         |*****""".stripMargin)
  }

  test("Elements displays as their chars") {
    val elems = Seq(
      Wall(0, 0),
      createSender(0, 1, Red), createSender(0, 2, Blue),
      createReciver(1, 1, Red), createReciver(1, 2, Blue),
      createConnector(2, 0)
    )
    val state = createState(3, 3, elems)
    assert(state.toString ==
      """|#RB
         |*rb
         |A**""".stripMargin)
  }

  test("Can't add beams to different colors") {
    val sender = createSender(0, 0, Red)
    val connector = createConnector(0, 2)
    val reciver = createReciver(2, 0, Blue)
    val state = createState(3, 3, Seq(sender, connector, reciver))

    state.addBeam(sender, connector)
    state.beams should have size 1

    state.addBeam(reciver, connector)
    state.beams should have size 1
  }

  test("Can't add beams duplicate beams") {
    val sender = createSender(0, 0, Red)
    val connector = createConnector(0, 1)
    val reciver = createReciver(1, 1, Blue)
    val state = createState(2, 2, Seq(sender, connector, reciver))

    assert(!state.isBeamExists(sender, connector))
    state.addBeam(sender, connector)
    state.beams should have size 1

    state.addBeam(sender, connector)
    state.beams should have size 1
  }

  test("Can't add beam to the same connector") {
    val connector = createConnector(0, 0)
    val state = createState(2, 2, Seq(connector))

    state.addBeam(connector, connector)
    state.beams should have size 0
  }

  test("Can't add intersecting beams") {
    val sender1 = createSender(0, 0, Red)
    val connector1 = createConnector(2, 2)
    val connector2 = createConnector(0, 2)
    val sender2 = createSender(2, 0, Red)

    val state = createState(3, 3, Seq(sender1, connector1, connector2, sender2))

    state.addBeam(sender1, connector1)
    state.beams should have size 1

    state.addBeam(sender2, connector2)
    state.beams should have size 1
  }

  test("Beam between empty conectrator and reciever should have absent color") {
    val connector = Connector(0, 2)()
    val reciver = createReciver(2, 0, Red)
    val state = createState(3, 3, Seq(connector, reciver))
    state.addBeam(reciver, connector)
    state.beams.head.color should be(Absent)
  }

  test("Beam between empty conectrators should have absent color") {
    val connector = createConnector(0, 2)
    val connector1 = createConnector(2, 0)
    val state = createState(3, 3, Seq(connector, connector1))
    state.addBeam(connector1, connector)
    state.beams.head.color should be(Absent)
  }

  test("Beams updates connectors colors") {
    val sender = createSender(0, 0, Red)
    val connector = createConnector(0, 1)
    val connector1 = createConnector(1, 1)

    val state = createState(3, 3, Seq(sender, connector))

    connector.color should be(Absent)
    connector1.color should be(Absent)

    state.addBeam(sender, connector)

    connector.color should be(Red)

    state.addBeam(connector1, connector)

    connector1.color should be(Red)
  }

  test("cant add beams, intersecting elements") {
    val sender = createSender(0, 0, Red)
    val connector = createConnector(0, 2)
    val wall = createWall(0, 1)
    val state = createState(1, 3, Seq(sender, connector, wall))

    state.addBeam(sender, connector)
    state.beams should have size 0
  }

  test("isWin works properly") {
    val sender = createSender(0, 0, Red)
    val connector = createConnector(1, 0)
    val reciver = createReciver(2, 0, Red)
    val targetReciever = createReciver(1, 1, Red, isTarget = true)
    val state = createState(3, 3, Seq(sender, connector, reciver, targetReciever))

    assert(!state.isWinState)

    state.addBeam(sender, connector)
    state.addBeam(reciver, connector)
    assert(!state.isWinState)

    state.addBeam(targetReciever, connector)
    assert(state.isWinState)
  }

  test("Absent beams can intersect env") {
    val reciver = createReciver(0, 0, Red)
    val connector = createConnector(0, 2)
    val wall = createWall(0, 1)
    val state = createState(1, 3, Seq(reciver, connector, wall))
    state.addBeam(reciver, connector)
  }

  test("Absent beam can intersect another beams") {
    val sender = createSender(0, 0, Red)
    val connector1 = createConnector(2, 2)
    val connector2 = createConnector(0, 2)
    val reciver = createReciver(2, 0, Red)

    val state = createState(3, 3, Seq(sender, connector1, connector2, reciver))

    state.addBeam(sender, connector1)
    state.addBeam(reciver, connector2)
  }

  test("Beams should propagate color to absent beams") {
    val sender = createSender(0, 0, Red)
    val connector1 = createConnector(0, 1)
    val connector2 = createConnector(0, 2)
    val connector3 = createConnector(0, 3)
    val reciver = createReciver(3, 3, Red, isTarget = true)

    val state = createState(4, 4, Seq(sender, connector1, connector2, connector3, reciver))

    state.addBeam(reciver, connector3)
    state.addBeam(connector2, connector3)

    assert(!state.isWinState)
    assert(state.beams.forall(_.color == Absent))

    state.addBeam(sender, connector1)
    state.addBeam(connector2, connector1)
    assert(state.beams.forall(_.color == Red))
    assert(state.isWinState)
  }

  test("Simple connect beam should have sender color") {
    val sender = createSender(0, 0, Red)
    val connector1 = createConnector(0, 1)
    val reciver = createReciver(1, 1, Red, isTarget = true)

    val state = createState(2, 2, Seq(sender, connector1, reciver))

    state.addBeam(sender, connector1)
    state.addBeam(reciver, connector1)
    assert(state.beams.forall(_.color == Red))
    assert(state.isWinState)
  }
}

