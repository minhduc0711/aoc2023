import scala.io.Source

val GAME_PATTERN = raw"Game (\d+): (.*)".r
val BALL_SET_PATTERN = raw"((\d+) (\w+))".r

def parseGames(s: String): Map[Int, Array[Map[String, Int]]] =
  GAME_PATTERN
    .findAllIn(s)
    .matchData
    .map(m => (m.group(1).toInt -> m.group(2).split(";").map(parseBallset(_))))
    .toMap

def parseBallset(s: String): Map[String, Int] =
  BALL_SET_PATTERN
    .findAllIn(s.strip)
    .matchData
    .map(m => (m.group(3) -> m.group(2).toInt))
    .toMap

def getMin(drawnCubeSets: Array[Map[String, Int]]) =
  drawnCubeSets.reduce((m1, m2) =>
    m1 ++ m2.map((k, v) => k -> Math.max(v, m1.getOrElse(k, 0)))
  )

def checkValidity(
    drawnCubeSet: Map[String, Int],
    fullCubeSet: Map[String, Int]
) =
  if ((drawnCubeSet.keySet -- fullCubeSet.keySet).size > 0)
    false
  else
    drawnCubeSet.keySet.forall(k => drawnCubeSet(k) <= fullCubeSet(k))

@main def day2() =
  val filename = "input/day2.txt"

  val text = Source.fromFile(filename).mkString
  val lines = text.split("\n")

  val fullSet = Map("red" -> 12, "green" -> 13, "blue" -> 14)

  val gameData = parseGames(text)
  val res = gameData.view
    .mapValues(cubeSets => cubeSets.forall(checkValidity(_, fullSet)))
    .filter((gameId, isValid) => isValid)
    .keys
    .reduce(_ + _)
  println("part 1")
  println(res)

  println("part 2")
  println(gameData.values.map(getMin(_).values.reduce(_ * _)).sum)
