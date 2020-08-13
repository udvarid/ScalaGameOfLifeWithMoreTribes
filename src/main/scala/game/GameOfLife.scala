package game

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

import scala.annotation.tailrec
import scala.util.Random

class GameOfLife(initCells: Int, tribes: Int, canvas: Canvas, size: Int) {

  private val tribeOfficial = tribes.min(4).max(1)
  private val cells: Set[Cell] = fillUpCells

  def fillUpCells: Set[Cell] = {

    val givePlaceForX: Int => Int = t => (t % 2 - 1).abs
    val givePlaceForY: Int => Int = t => if (t == 1 || t == 4) 0 else 1

    @tailrec
    def createCells(cellsInProgress: Set[Cell]): Set[Cell] = {
      if (cellsInProgress.size == initCells * tribes) cellsInProgress
      else {
        val tribe = Random.nextInt(tribeOfficial) + 1
        val coordX = Random.nextInt(size / 25 / 2) + size / 25 * givePlaceForX(tribe)
        val coordY = Random.nextInt(size / 25 / 2) + size / 25 * givePlaceForY(tribe)
        createCells(cellsInProgress + Cell(coordX, coordY, tribe))
      }
    }

    createCells(Set[Cell]())
  }

  def startGame: Unit = {
    for (i <- 1 to tribeOfficial) {
      cells.filter(cell => cell.tribe == i).foreach(println)
      println("-------")
    }
  }

}
