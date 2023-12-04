import scala.io.Source

@main def day1() =
  val filename = "input/day1.txt"

  val wordsToNum = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  // Use lookahead assertion to deal with overlapping words
  val pattern = s"""(?=(${wordsToNum.keySet mkString "|"}|\\d))""".r;

  def strToInt(s: String): Int =
    if (s.length == 1)
      s.toInt
    else
      wordsToNum(s)

  def computeCalibrationVal(s: String): Int =
    val matches = pattern.findAllIn(s).matchData.map(m => m.group(1)).toList;
    val first = strToInt(matches.head);
    val last = strToInt(matches.last);
    first * 10 + last


  val text = Source.fromFile(filename).mkString
  val lines: Array[String] = text.split("\n")

  val calibrationVals = lines.map(computeCalibrationVal(_))
  println(calibrationVals.reduce(_ + _))
