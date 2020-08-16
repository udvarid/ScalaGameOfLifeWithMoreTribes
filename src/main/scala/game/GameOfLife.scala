package game

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

import scala.+:
import scala.annotation.tailrec
import scala.util.Random

class GameOfLife(initCells: Int, tribes: Int, canvas: Canvas, size: Int) {

  private val tribeOfficial = tribes.min(4).max(1)
  private var cells: Set[Cell] = fillUpCells

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

  private def drawCells(newBurns: Set[Cell]): Unit = {
    val gc = canvas.graphicsContext2D
    newBurns.foreach(c => {
      gc.fill = giveTribeColor(c.tribe)
      val fold = 10
      if (c.tribe == 1 || c.tribe == 2) gc.fillOval(c.x * 25 + 7, c.y * 25 + 7, fold, fold)
      else gc.fillRect(c.x * 25 + 7, c.y * 25 + 7, fold, fold)
    })
  }

  private def clearDeadCells(corps: Set[Cell]): Unit = {
    val gc = canvas.graphicsContext2D
    corps.foreach(c => {
      gc.fill = Color.White
      val fold = 11
      if (c.tribe == 1 || c.tribe == 2) gc.fillOval(c.x * 25 + 7, c.y * 25 + 7, fold, fold)
      else gc.fillRect(c.x * 25 + 7, c.y * 25 + 7, fold, fold)
    })
  }


  implicit class PimpMyInteger(number: Int) {
    def isDying: Boolean = number < 2 || number > 3

    def canBorn: Boolean = number == 3

    def isInside: Boolean = number >= 0 && number < size / 25
  }


  def startGame(): Unit = {
    drawCells(cells)

    @tailrec
    def nextRound(): Unit = {
      Thread.sleep(500)
      if (cells.isEmpty) return
      else {
        //natural death of cells
        val deadCells: Set[Cell] = cells.filter(c => cells.count(c2 => c2 ~ c).isDying)

        //war effect
        var warCasualities: Set[Cell] = Set[Cell]()
        cells.foreach(c => {
          val conflictZones: Set[Cell] = cells.filter(c2 => c2 closeEnemy c) + c

          if (conflictZones.nonEmpty) {
            conflictZones.foreach(conflCell => {
              cells.foreach(clearCell => clearCell.visited = false)

              def findGroupStrength(checkThis: Cell, value: Int): Int = {
                if (!checkThis.visited) {
                  checkThis.visited = true

                  val upCell: Option[Cell] = cells.find(fc => fc.x == checkThis.x + 1 && fc.y == checkThis.y && fc.tribe == checkThis.tribe)
                  val downCell: Option[Cell] = cells.find(fc => fc.x == checkThis.x - 1 && fc.y == checkThis.y && fc.tribe == checkThis.tribe)
                  val leftCell: Option[Cell] = cells.find(fc => fc.x == checkThis.x && fc.y == checkThis.y + 1 && fc.tribe == checkThis.tribe)
                  val rightCell: Option[Cell] = cells.find(fc => fc.x == checkThis.x && fc.y == checkThis.y - 1 && fc.tribe == checkThis.tribe)

                  val upValue: Int = if (upCell.isEmpty) value else findGroupStrength(upCell.get, value + 1)
                  val downValue: Int = if (downCell.isEmpty) value else findGroupStrength(downCell.get, value + 1)
                  val leftValue: Int = if (leftCell.isEmpty) value else findGroupStrength(leftCell.get, value + 1)
                  val rightValue: Int = if (rightCell.isEmpty) value else findGroupStrength(rightCell.get, value + 1)

                  upValue.max(downValue).max(leftValue).max(rightValue)

                } else value
              }
              conflCell.value = findGroupStrength(conflCell, 1)
            })
            val maxValue = conflictZones.map(cellToValue => cellToValue.value).max
            warCasualities = warCasualities ++ conflictZones.filter(cas => cas.value < maxValue)

          }

        })

        if (warCasualities.nonEmpty) {
          println(s"In this turn ${warCasualities.size} would be dead")
          warCasualities.foreach(println)
        }

        //potential newburns
        val newBurns: Set[Cell] = cells.flatMap(c => (!c).filter(cf => cf.x.isInside && cf.y.isInside))
        //filter newburns - already occupied
        val newBurnsFilter1: Set[Cell] = newBurns.filter(nc => !cells.exists(c => c.x == nc.x && c.y == nc.y))
        //filter newburns - different tribes on the same coordinate
        val newBurnsFilter2: Set[Cell] = newBurnsFilter1
          .filter(c => !newBurnsFilter1.exists(c2 => c.x == c2.x && c.y == c2.y && c.tribe != c2.tribe))
        //filter newburns - not ready to live
        val newBurnsFilter3: Set[Cell] = newBurnsFilter2
          .filter(nc => cells.count(c => c ~ nc).canBorn)
        drawCells(newBurnsFilter3)
        //add newburns to cells
        cells = cells ++ newBurnsFilter3

        //kill the natural deads
        clearDeadCells(deadCells)
        cells = cells.diff(deadCells)

        //kill the war deads
        clearDeadCells(warCasualities)
        cells = cells.diff(warCasualities)


        for (tribe <- 1 to cells.map(c => c.tribe).max) {
          val numbOfCells = cells.count(c => c.tribe == tribe)
          print(s"Tribe $tribe : $numbOfCells - ")
        }
        println()


        nextRound()
      }
    }

    nextRound()
  }

}
