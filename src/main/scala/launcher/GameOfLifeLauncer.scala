package launcher

import game.GameOfLife
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color


object GameOfLifeLauncer extends JFXApp {
  val size: Int = 750
  val canvas = new Canvas(size, size)
  val gc = canvas.graphicsContext2D

  gc.fill = Color.Green
  gc.stroke = Color.Green
  gc.lineWidth = 2
  gc.strokeLine(0, 0, 0, size - 1)
  gc.strokeLine(0, size - 1, size - 1, size - 1)
  gc.strokeLine(size - 1, size - 1, size - 1, 0)
  gc.strokeLine(size - 1, 0, 0, 0)
  gc.lineWidth = 1
  for (x <- 1 to size / 25) {
    gc.strokeLine(x * 25, 0, x * 25, size)
    gc.strokeLine(0, x * 25, size, x * 25)
  }


  val aThread = new Thread(() =>  {
    val game = new GameOfLife(50, 4, canvas, size)
    game.startGame
  })

  aThread.start() // start a paralell thread

  stage = new PrimaryStage {
    title = "Game Of Life scene"
    scene = new Scene {
      content = canvas
    }
  }


}
