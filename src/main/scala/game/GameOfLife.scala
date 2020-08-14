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
        val coordX = Random.nextInt(size / 50) + size / 50 * givePlaceForX(tribe)
        val coordY = Random.nextInt(size / 50) + size / 50 * givePlaceForY(tribe)
        createCells(cellsInProgress + Cell(coordX, coordY, tribe))
      }
    }

    createCells(Set[Cell]())
  }

  val giveTribeColor: Int => Color = {
    case 1 => Color.Green
    case 2 => Color.Red
    case 3 => Color.Blue
    case 4 => Color.Purple
  }

  val giveColor: Cell => Color = a => if (a.live) giveTribeColor(a.tribe) else Color.White

  val giveFold: PartialFunction[Boolean, Int] = {
    case true => 10
    case false => 11
  }

  private def drawCells(cellsToDraw: Set[Cell]): Unit = {
    val gc = canvas.graphicsContext2D
    cellsToDraw.filter(c => c.newBorn || !c.live)
      .foreach(c => {
        gc.fill = giveColor(c)
        val fold = giveFold(c.live)
        if (c.tribe == 1 || c.tribe == 2) gc.fillOval(c.x * 25 + 7, c.y * 25 + 7, fold, fold)
        else gc.fillRect(c.x * 25 + 7, c.y * 25 + 7, fold, fold)
        if (c.newBorn) c.newBorn = false
      })
  }

  implicit class PimpMyInteger(number: Int) {
    def isDying: Boolean = number < 2 || number > 3
    def canBorn: Boolean = number == 3
    def isInside: Boolean = number >= 0 && number < size / 25
  }

  def passAway(workingCells: Set[Cell]): Unit = {
    workingCells.foreach(c => {
      if (workingCells.count(c2 => c2 ~ c).isDying) c.diePlease()
    })
  }

  def newBurnAdded(workingCells: Set[Cell]): Set[Cell] = {

    val newBurns = workingCells.flatMap(c => (!c).filter(cf => cf.x.isInside && cf.y.isInside))

    @tailrec
    def selectNewBurns(newBurnCells: Set[Cell], livingCells: Set[Cell]): Set[Cell] = {
      if (newBurnCells.isEmpty) livingCells
      else {
        if (livingCells.count(c2 => c2 ~ newBurnCells.head).canBorn)
          selectNewBurns(newBurnCells.toList.tail.toSet, livingCells + newBurnCells.head)
        else
          selectNewBurns(newBurnCells.toList.tail.toSet, livingCells)
      }
    }

    selectNewBurns(newBurns.diff(workingCells), workingCells)

  }

  def startGame(): Unit = {
    drawCells(cells)

    @tailrec
    def nextRound(workingCells: Set[Cell]): Unit = {
      Thread.sleep(1500)
      if (workingCells.isEmpty) return
      else {
        passAway(workingCells)
        val extendedCells = newBurnAdded(workingCells)
        drawCells(extendedCells)
        nextRound(extendedCells.filter(c => c.live))
      }
    }

    nextRound(cells)
  }

}
