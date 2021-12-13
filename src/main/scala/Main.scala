import scala.io.Source

@main def main =
  Day1.run
  Day2.run

object Day1:

  def run: Unit =
    part1
    part2

  def part1: Unit =
    val input  = Source.fromFile("input/day1").mkString
    val answer = part1(input)

    println(s"The solution for Day1 part1 is $answer")

  def part2: Unit =
    val input  = Source.fromFile("input/day1").mkString
    val answer = part2(input)

    println(s"The solution for Day1 part2 is $answer")

  def part1(input: String): Int =
    val depths = input.linesIterator.map(_.toInt)
    val pairs  = depths.sliding(2).map(arr => (arr(0), arr(1)))

    pairs.count((prev, next) => prev < next)

  def part2(input: String): Int =
    val depths = input.linesIterator.map(_.toInt)
    val sums   = depths.sliding(3).map(_.sum)
    val pairs  = sums.sliding(2).map(arr => (arr(0), arr(1)))

    pairs.count((prev, next) => prev < next)

object Day2:

  def run: Unit =
    part1

  def part1: Unit =
    val input = Source.fromFile("input/day2").mkString
    val moves = input.linesIterator.toList
      .map(_.split(" ").toList)
      .map {
        case "forward" :: value :: Nil => Move.Forward(value.toInt)
        case "down" :: value :: Nil    => Move.Down(value.toInt)
        case "up" :: value :: Nil      => Move.Up(value.toInt)
        case _                         => throw new IllegalStateException
      }

    val positions = moves
      .foldLeft((0, 0)) { (acc, move) =>
        move match
          case Move.Forward(value) => (acc._1 + value, acc._2)
          case Move.Down(value)    => (acc._1, acc._2 + value)
          case Move.Up(value)      => (acc._1, acc._2 - value)
      }

    println(s"The solution for Day2 is ${positions._1 * positions._2}")

  enum Move:
    case Forward(value: Int) extends Move
    case Down(value: Int) extends Move
    case Up(value: Int) extends Move
