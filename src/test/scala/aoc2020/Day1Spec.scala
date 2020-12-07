package aoc2020

import org.scalatest._
import flatspec._
import matchers._

class SumFinderSpec extends AnyFlatSpec with should.Matchers {

  import SumFinder._

  "SumFinder" should "find the two list entries that add up to a target" in {
    val xs: List[Int] = List(
      1721,
      979,
      366,
      299,
      675,
      1456
      )
    val result = findPairSum(xs, 2020) 
    assert(result._1 == 1721)
    assert(result._2 == 299)
  }

  "SumFinder" should "find the three list entries that add up to a target" in {
    val xs: List[Int] = List(
      1721,
      979,
      366,
      299,
      675,
      1456
      )
    val result = findTripletSum(xs, 2020) 
    assert(result._1 == 979)
    assert(result._2 == 366)
    assert(result._3 == 675)
  }

}

