package game

case class Cell(x: Int, y: Int, tribe: Int) {

  def ~(cell: Cell): Boolean =
    cell != this && cell.tribe == this.tribe && (x - cell.x).abs <= 1 && (y - cell.y).abs <= 1

  def unary_! : Set[Cell] =
    Set[Cell](Cell(x - 1, y - 1, this.tribe), Cell(x, y - 1, this.tribe), Cell(x + 1, y - 1, this.tribe),
      Cell(x - 1, y + 1, this.tribe), Cell(x, y + 1, this.tribe), Cell(x + 1, y + 1, this.tribe),
      Cell(x - 1, y, this.tribe), Cell(x + 1, y, this.tribe))

}
