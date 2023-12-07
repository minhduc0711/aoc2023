import scala.io.Source
import scala.math
import scala.collection.mutable.ArrayBuffer



// def getSurroundingIndices(row: Int, numStart: Int, numEnd: Int, numRows: Int, numCols: Int): List[Tuple2[Int, Int]] =
//   val colStart = Math.max(0, numStart - 1)
//   val colEnd = Math.max(math.
//   val top = if (row > 0) List.fill(rectLen)(row - 1).zip(List.range(colStart - 0, colEnd + 1)) else List.empty
//   top

@main def day3() =
  val filename = "input/day3.txt"

  val text = Source.fromFile(filename).mkString
  val lines = text.split("\n").toArray;

  val number_pattern = raw"\d+".r
  val gear_pattern = raw"\*".r

  val mat = lines.map(_.toList.toArray);
  val numRows = mat.size
  val numCols = mat(0).size

  val partNumbers = ArrayBuffer.empty[Int]
  for ((line, row) <- lines.zipWithIndex)
    for (m <- number_pattern.findAllIn(line).matchData)
      val colStart = math.max(0, m.start - 1)
      val colEnd = math.min(numCols, m.end + 1)

      val top = if (row > 0) List.fill(colEnd - colStart)(row - 1).zip(List.range(colStart, colEnd + 1)) else List.empty
      val bottom = if (row < numRows - 1) List.fill(colEnd - colStart)(row + 1).zip(List.range(colStart, colEnd + 1)) else List.empty
      val left = if (m.start - 1 >= 0) List((row, m.start - 1)) else List.empty
      val right = if (m.end < numCols) List((row, m.end)) else List.empty

      val neighborPositions = top ::: left ::: right ::: bottom
      if (neighborPositions.map((i, j) => mat(i)(j)).filter(ch =>(!ch.isDigit && ch != '.')).size > 0)
        partNumbers += m.toString.toInt

  println(partNumbers.reduce(_ + _))


  for ((line, row) <- lines.zipWithIndex)
    for (m <- gear_pattern.findAllIn(line).matchData)
      val colStart = math.max(0, m.start - 1)
      val colEnd = math.min(numCols, m.end + 1)
      println(s"$row ${m.start}")

      // val top = if (row > 0) List.fill(colEnd - colStart)(row - 1).zip(List.range(colStart - 0, colEnd + 1)) else List.empty
      // val bottom = if (row < numRows - 1) List.fill(colEnd - colStart)(row + 1).zip(List.range(colStart - 0, colEnd + 1)) else List.empty
      // val left = if (m.start - 1 >= 0) List((row, m.start - 1)) else List.empty
      // val right = if (m.end < numCols) List((row, m.end)) else List.empty

      val numPartNoAbove = if (row > 0) lines(row -1).slice(colStart, colEnd).split('.').filter(!_.isEmpty).size else 0
      val numPartNoBelow = if (row < numRows - 1) lines(row + 1).slice(colStart, colEnd).split('.').filter(!_.isEmpty).size else 0
      val numPartNoLeft = if ((m.start - 1 >= 0) && (line(m.start - 1).isDigit)) 1  else 0
      val numPartNoRight = if ((m.end < numCols) && (line(m.end).isDigit)) 1 else 0

      val numNeighborPartNo = numPartNoAbove + numPartNoBelow + numPartNoLeft + numPartNoRight
      // println(numNeighborPartNo)
      if (numNeighborPartNo == 2)
        println("yay")


      // val adjNumberCount = 0
      // adjNumberCount += top.
