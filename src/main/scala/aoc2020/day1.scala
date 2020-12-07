package aoc2020

import scala.io.Source

/** https://adventofcode.com/2020/day/1 */
object SumFinder extends App {

  def findPairSum(xs: List[Int], target: Int): (Int, Int) = {
    val solutions = for {
      x <- xs
      y <- xs
      if (x != y) && (x + y == target)
    } yield (x, y)
    solutions.head
  }

  def findTripletSum(xs: List[Int], target: Int): (Int, Int, Int) = {
    val solutions = for {
      x <- xs
      y <- xs
      z <- xs
      if (x != y) && (x != z) && (x + y + z == target)
    } yield (x, y, z)
    solutions.head
  }

  val xs: List[Int] = 
    Source.fromResource("2020/day1.txt").getLines
      .map((s: String) => s.toInt)
      .toList
  val pair = findPairSum(xs, 2020)
  val pairResult = pair._1 * pair._2
  println(s"The result is: ${pairResult}")

  val triplet = findTripletSum(xs, 2020)
  val tripletResult = triplet._1 * triplet._2 * triplet._3
  println(s"The result is: ${tripletResult}")
}
