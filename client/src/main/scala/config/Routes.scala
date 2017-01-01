package config

import scalatags.generic.Attr

object Routes {
  object Game {
    val base = "/game"

    def gameData(id: Int) = base + s"/$id/data"
    def game(id: Int) = base + s"/$id"
    def updateState = base + "/state"
  }

}
