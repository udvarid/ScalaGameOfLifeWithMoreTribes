package game

case class Cell(x: Int, y: Int, tribe: Int = 1,
                var live: Boolean = true,
                var newBorn: Boolean = true) {

  def ~(cell: Cell): Boolean =
    cell != this && (x - cell.x).abs <= 1 && (y - cell.y).abs <= 1 && !newBorn

  def diePlease = this.live = false

  def unary_! : Set[Cell] =
    Set[Cell](Cell(x - 1, y - 1), Cell(x, y - 1), Cell(x + 1, y - 1),
      Cell(x - 1, y + 1), Cell(x, y + 1), Cell(x + 1, y + 1),
      Cell(x - 1, y), Cell(x + 1, y))

}
