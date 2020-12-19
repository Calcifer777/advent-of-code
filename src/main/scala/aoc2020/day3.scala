package aoc2020

import scala.io.Source


object TreeFinder extends App {

  def treeCount(route: List[String], rowShift: Int, colShift: Int): Int = {
    def loop(route: List[String], count: Int, row: Int, col: Int): Int = {
      if (row >= route.length)
        count
      else {
        val isTree = if (route(row)(col) == '#') 1 else 0
        loop(route, count+isTree, row+rowShift, (col+colShift)%route(0).length)
      }
    }
    loop(route, 0, 0, 0)
  }

  val route = Source.fromResource("2020/day3.txt").getLines.toList
  val trees = treeCount(route, 1, 3)
  println(s"Number of trees found: $trees")

  val slopes = List(
    (1, 1),
    (1, 3),
    (1, 5),
    (1, 7),
    (2, 1)
    )
  val part2Result = for {
    (rowShift, colShift) <- slopes
  } yield treeCount(route, rowShift, colShift)
  println(part2Result.product)
}
