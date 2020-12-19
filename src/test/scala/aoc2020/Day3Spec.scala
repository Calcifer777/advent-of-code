package aoc2020


import org.scalatest._
import flatspec._
import matchers._

import TreeFinder._

class TreeFinderSpec extends AnyFlatSpec with should.Matchers with GivenWhenThen {

  "TreeFinder" should "find 7 trees in test route with shift (1, 3)" in {
    val route = """..##.......
                   |#...#...#..
                   |.#....#..#.
                   |..#.#...#.#
                   |.#...##..#.
                   |..#.##.....
                   |.#.#.#....#
                   |.#........#
                   |#.##...#...
                   |#...##....#
                   |.#..#...#.#""".stripMargin('|').split("\n").toList
    val trees = treeCount(route, 1, 3)
    assert(trees == 7)
  }
}
