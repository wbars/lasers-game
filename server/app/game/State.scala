package game

import java.awt.geom.Line2D

import game.State.Component

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

  def transparent: Boolean = false
}

sealed trait Colored extends Element {
  def color: Color

  def beams: mutable.Set[Beam]
}

case class Sender(override val x: Int, override val y: Int, override val color: Color)(val beams: mutable.Set[Beam] = mutable.Set.empty) extends Colored {
  override def ch: Char = color match {
    case Red => 'R'
    case Blue => 'B'
  }
}

case class Reciver(override val x: Int, override val y: Int, override val color: Color, var isTarget: Boolean = false)(val beams: mutable.Set[Beam] = mutable.Set.empty, val wires: mutable.Set[Wall] = mutable.Set.empty) extends Colored {
  override def ch: Char = color match {
    case Red => 'r'
    case Blue => 'b'
  }

  def active: Boolean = beams.exists(_.color == color)
}


case class Wall(override val x: Int, override val y: Int, wires: mutable.Set[Reciver] = mutable.Set.empty, jammers: mutable.Set[Jammer] = mutable.Set.empty) extends Element {
  override def transparent: Boolean = jammers.exists(_.active) || (wires.nonEmpty && wires.forall(_.active))

  override def ch: Char = if (transparent) '*' else '#'
}

case class Jammer(var x: Int, var y: Int, override val ch: Char = 'J')(var target: Option[Wall] = None, var active: Boolean = false) extends Element

case class Connector(var x: Int, var y: Int, override val ch: Char = 'A')(val beams: mutable.Set[Beam] = mutable.Set.empty) extends Colored {
  def color: Color = beams.collectFirst({ case b: Beam if b.color != Absent => b.color }).getOrElse(Absent)
}

sealed class Line(element1: Element, element2: Element) {
  def x1: Double = element1.x + 0.5

  def y1: Double = element1.y + 0.5

  def x2: Double = element2.x + 0.5

  def y2: Double = element2.y + 0.5

  def apply(element1: Element, element2: Element): Line = new Line(element1, element2)
}

case class Beam(colored: Colored, connector: Connector)(var color: Color) extends Line(element1 = colored, element2 = connector)

class State(val width: Int, val height: Int,
            val elements: mutable.Map[(Int, Int), Element],
            val beams: mutable.Set[Beam] = mutable.Set.empty) {
  def beamsIntersectsElement(element: Element): mutable.Set[Beam] = beams.filter(State.isLineIntersectsElement(_, element))

  def isValid = true

  def jammers: Iterable[Jammer] = elements.values.collect({ case j: Jammer => j })

  def walls: Iterable[Wall] = elements.values.collect({ case w: Wall => w })

  override def toString: String = {
    def buildRow(i: Int) = {
      Range(0, width).map(j => elements.getOrElse((i, j), None) match {
        case e: Element => e.ch
        case _ => '*'
      }).mkString
    }

    Range(0, height).map(buildRow).mkString("\n")
  }

  def addBeam(colored: Colored, connector: Connector): Beam = {

    def isBeamValid(beam: Beam): Boolean = {
      if (connector == colored) return false
      if (colored.color != Absent && connector.color != Absent && colored.color != connector.color) return false
      if (isBeamExists(colored, connector)) return false

      true
    }

    val beam = Beam(colored, connector)(Absent)
    if (isBeamValid(beam)) {
      addBeam(beam)
    }
    beam
  }

  def removeBeam(beam: Beam) {
    beam.colored.beams -= beam
    beam.connector.beams -= beam
    beams -= beam

    paintBeamElementsComponents(beam)
  }

  def paintBeamElementsComponents(beam: Beam, paintedRecivers: Set[Reciver] = Set.empty) {
    if (isBeamIntersectsEnv(beam)) beam.color = Absent
    paintComponent(getConnectedComponent(beam.colored), paintedRecivers)
    paintComponent(getConnectedComponent(beam.connector), paintedRecivers)
  }

  def isBeamExists(first: Colored, second: Colored): Boolean = beams.exists(e => Set(e.colored, e.connector) == Set(first, second))

  def isWinState: Boolean = elements.values.collect({ case r: Reciver if r.isTarget => r }).forall(_.active)

  def paintComponent(component: Component, paintedRecivers: Set[Reciver] = Set.empty) {
    val componentColor = component.elements.find({
      case _: Sender => true
      case _ => false
    }) match {
      case Some(s: Sender) => s.color
      case _ => Absent
    }
    component.beams.foreach(_.color = componentColor)

    val recivers = component.elements.collect({ case r: Reciver => r })
      .filter(!paintedRecivers.contains(_)).toSet
    recivers
      .flatMap(_.wires)
      .flatMap(beamsIntersectsWall)
      .foreach(paintBeamElementsComponents(_, recivers ++ paintedRecivers))
  }

  def getConnectedComponent(colored: Colored): Component = {
    val componentElements: mutable.Set[Colored] = mutable.Set.empty[Colored]
    val componentBeams: mutable.Set[Beam] = mutable.Set.empty[Beam]

    def dfs(colored: Colored) {
      componentElements += colored
      colored.beams
        .filter(!isBeamIntersectsEnv(_))
        .foreach(beam => {
          componentBeams += beam
          if (!componentElements.contains(beam.colored)) dfs(beam.colored)
          if (!componentElements.contains(beam.connector)) dfs(beam.connector)
        })
    }

    dfs(colored)
    Component(componentElements, componentBeams)
  }

  private def beamsIntersectsWall(wall: Wall): mutable.Set[Beam] = beams.filter(State.isLineIntersectsElement(_, wall))

  private def isBeamIntersectsEnv(beam: Beam): Boolean = elements.values
    .exists(e => e != beam.colored && e != beam.connector && !e.transparent && State.isLineIntersectsElement(beam, e)) ||
    beams.exists(b => b != beam && State.isLinesIntersects(b, beam))

  def isLineIntersectWall(elem1: Element, elem2: Element): Boolean =
    walls.exists(w => !Set(elem1, elem2).contains(w) && !w.transparent && State.isLineIntersectsElement(new Line(elem1, elem2), w))

  private def addBeam(beam: Beam) = {
    beams += beam
    beam.connector.beams += beam
    beam.colored.beams += beam

    paintBeamElementsComponents(beam)
  }

  def beamsIntersectsBeam(beam: Beam): mutable.Set[Beam] = beams.filter(State.isLinesIntersects(_, beam))
}

object State {

  case class Component(elements: Iterable[Colored], beams: Iterable[Beam])

  def isLinesIntersects(first: Line, second: Line): Boolean = {
    val firstPoints = Set((first.x1, first.y1), (first.x2, first.y2))
    val isLinesTouches = firstPoints.contains((second.x1, second.y1)) || firstPoints.contains((second.x2, second.y2))
    !isLinesTouches && Line2D.linesIntersect(first.x1, first.y1, first.x2, first.y2, second.x1, second.y1, second.x2, second.y2)
  }

  def isLineIntersectsElement(line: Line, element: Element): Boolean =
    Line2D.linesIntersect(line.x1, line.y1, line.x2, line.y2, element.x, element.y, element.x + 1, element.y) ||
      Line2D.linesIntersect(line.x1, line.y1, line.x2, line.y2, element.x + 1, element.y, element.x + 1, element.y + 1) ||
      Line2D.linesIntersect(line.x1, line.y1, line.x2, line.y2, element.x + 1, element.y + 1, element.x, element.y + 1) ||
      Line2D.linesIntersect(line.x1, line.y1, line.x2, line.y2, element.x, element.y + 1, element.x, element.y)
}