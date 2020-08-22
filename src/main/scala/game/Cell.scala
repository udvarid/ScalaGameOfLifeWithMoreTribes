package game

import scala.util.Random

case class Cell(x: Int, y: Int, tribe: Int, var value: Int = 1, var visited: Boolean = false) {

  val aggression: Int = tribe match {
    case 1 => 100
    case 2 => 25
    case 3 => 25
    case 4 => 25
  }

  val chanceOfBirth: Double = Random.nextInt(this.aggression) / 100

  def ~(cell: Cell): Boolean =
    cell != this && cell.tribe == this.tribe && (x - cell.x).abs <= 1 && (y - cell.y).abs <= 1

  def closeEnemy(cell: Cell): Boolean =
    cell != this && cell.tribe != this.tribe && (x - cell.x).abs <= 1 && (y - cell.y).abs <= 1


  def unary_! : Set[Cell] =
    Set[Cell](Cell(x - 1, y - 1, this.tribe), Cell(x, y - 1, this.tribe), Cell(x + 1, y - 1, this.tribe),
      Cell(x - 1, y + 1, this.tribe), Cell(x, y + 1, this.tribe), Cell(x + 1, y + 1, this.tribe),
      Cell(x - 1, y, this.tribe), Cell(x + 1, y, this.tribe))

}
