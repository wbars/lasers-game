package game

import scalatags.JsDom.all.{background, border, height, textAlign, width, _}
import scalatags.stylesheet.{CascadingStyleSheet, Cls}

object GameGridStyleSheet extends CascadingStyleSheet {
  val gameGrid: Cls = cls(
    width := "500px",
    height := "300px",
    tr(
      td(
        border := "1px solid black",
        width := "15px",
        height := "15px",
        textAlign := "center",
        &.hover(
          background := "black",
          color := "white",
          cursor := "pointer"
        )
      )
    )
  )

  val active: Cls = cls(
    background := "black",
    color := "white"
  )

  val selected: Cls = cls(
    border := "1px dotted black",
    background := "grey"
  )

  val beam: Cls = cls(
    padding := "0px",
    margin := "0px",
    height := "1px",
    backgroundColor := "black",
    lineHeight := "1px",
    position := "absolute"
  )

  val wall: Cls = cls(
    fontWeight := "bold"
  )
}
