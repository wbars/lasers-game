package game

import java.awt.geom.Line2D

import scala.collection.mutable

sealed abstract class Color

case object Red extends Color

case object Blue extends Color

case object Absent extends Color

sealed trait Element {
  def x: Int

  def y: Int

  def ch: Char

  override def toString: String = ch.toString
}

sealed trait Colored extends Element {
  def color: Color

  def beams: mutable.Set[Beam]
}

case class Wall(override val x: Int, override val y: Int, override val ch: Char = '#') extends Element

case class Sender(override val x: Int, override val y: Int, override val color: Color)(val beams: mutable.Set[Beam] = mutable.Set.empty) extends Colored {
  override def ch: Char = color match {
    case Red => 'R'
    case Blue => 'B'
  }
}

case class Reciver(override val x: Int, override val y: Int, override val color: Color, isTarget: Boolean = false)(val beams: mutable.Set[Beam] = mutable.Set.empty) extends Colored {
  override def ch: Char = color match {
    case Red => 'r'
    case Blue => 'b'
  }
}

case class Connector(var x: Int, var y: Int, override val ch: Char = 'A')(val beams: mutable.Set[Beam] = mutable.Set.empty) extends Colored {
  def color: Color = beams.collectFirst({ case b: Beam if b.color != Absent => b.color }).getOrElse(Absent)

}

case class Beam(colored: Colored, connector: Connector)(var color: Color) {
  def x1: Double = colored.x + 0.5

  def y1: Double = colored.y + 0.5

  def x2: Double = connector.x + 0.5

  def y2: Double = connector.y + 0.5

  def updateColor() {
    color = State.beamColor(colored, connector)
  }
}

class State(val width: Int, val height: Int, val elements: mutable.Map[(Int, Int), Element], val beams: mutable.Set[Beam] = mutable.Set.empty) {
  def isValid = true

  override def toString: String = {
    def buildRow(i: Int) = {
      Range(0, width).map(j => elements.getOrElse((i, j), None) match {
        case e: Element => e.ch
        case _ => '*'
      }).mkString
    }

    Range(0, height).map(buildRow).mkString("\n")
  }

  def isBeamIntersectsElement(element: Element, beam: Beam): Boolean =
    isBeamIntersectsLine(beam, element.x, element.y, element.x + 1, element.y) ||
      isBeamIntersectsLine(beam, element.x + 1, element.y, element.x + 1, element.y + 1) ||
      isBeamIntersectsLine(beam, element.x + 1, element.y + 1, element.x, element.y + 1) ||
      isBeamIntersectsLine(beam, element.x, element.y + 1, element.x, element.y)

  def isBeamIntersectsEnv(beam: Beam): Boolean = elements.values
    .exists(e => e != beam.colored && e != beam.connector && isBeamIntersectsElement(e, beam)) ||
    beams.exists(b => b != beam && isBeamsIntersects(b, beam))

  private def propagateColor(colored: Colored): Unit = {
    val propagatedBeams = mutable.Set.empty[Beam]

    def propagate(colored: Colored): Unit = colored match {
      case c: Connector => c.beams
        .filter(!propagatedBeams.contains(_))
        .filter(_.color == Absent)
        .foreach(beam => {
          beam.updateColor()
          propagatedBeams += beam

          propagate(beam.colored)
          propagate(beam.connector)
        })
      case _ =>
    }

    propagate(colored)
  }

  def addBeam(colored: Colored, connector: Connector) {

    def isBeamValid(beam: Beam): Boolean = {
      if (connector == colored) return false
      if (beam.color != Absent && isBeamIntersectsEnv(beam)) return false
      if (colored.color != Absent && connector.color != Absent && colored.color != connector.color) return false
      if (isBeamExists(colored, connector)) return false

      true
    }

    val beam = Beam(colored, connector)(State.beamColor(colored, connector))
    if (isBeamValid(beam)) {
      addBeam(colored, connector, beam)
      propagateColor(connector)
      propagateColor(colored)
    }
  }

  def removeBeam(beam: Beam) {
    beam.colored.beams.remove(beam)
    beam.connector.beams.remove(beam)
    beams.remove(beam)
  }


  private def addBeam(colored: Colored, connector: Connector, beam: Beam) = {
    beams += beam
    connector.beams += beam
    colored.beams += beam
  }

  def isBeamsIntersects(first: Beam, second: Beam): Boolean = {
    val firstPoints = Set((first.x1, first.y1), (first.x2, first.y2))
    val isLinesTouches = firstPoints.contains((second.x1, second.y1)) || firstPoints.contains((second.x2, second.y2))
    !isLinesTouches && isBeamIntersectsLine(first, second.x1, second.y1, second.x2, second.y2)
  }

  private def isBeamIntersectsLine(beam: Beam, x1: Double, y1: Double, x2: Double, y2: Double) =
    Line2D.linesIntersect(beam.x1, beam.y1, beam.x2, beam.y2, x1, y1, x2, y2)

  def isBeamExists(first: Colored, second: Colored): Boolean = beams.exists(e => Set(e.colored, e.connector) == Set(first, second))

  def isWinState: Boolean = elements.values.forall({
    case r: Reciver if r.isTarget => r.beams.exists(_.color == r.color)
    case _ => true
  })
}

object State {
  def beamColor(colored: Colored, connector: Connector): Color = connector.color match {
    case Absent => colored match {
      case _: Reciver => Absent
      case c: Colored => c.color
    }
    case _ => connector.color
  }
}