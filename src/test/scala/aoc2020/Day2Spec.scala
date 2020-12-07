package aoc2020

import org.scalatest._
import flatspec._
import matchers._

class PasswordFinderSpec extends AnyFlatSpec with should.Matchers with GivenWhenThen {

  import PasswordFinder._

  "PasswordFinder" should "parse an input string correctly" in {
    val pattern = raw"(\d)-(\d+) (\w): (\w+)"
    val result = parseInput("1-3 a: abcde", pattern)
    assert(result._1 == (1, 3, 'a'))
    assert(result._2 == "abcde")
  }

  it should "yield a pass result according to spec 1" in {
    Given("a policy-compliant password")
    val policy = (1, 3, 'a')
    val password = "abcde"
    assert(isCompliant(password, policy))
  }
  
  it should "yield a fail result according to spec 1" in {
    Given("a not-policy-compliant password") 
    val policy = (1, 3, 'b')
    val password = "cdefg"
    assert(!isCompliant(password, policy))

  }
  
  it should "yield a pass result according to spec 2" in {
    Given("a policy-compliant")
    val policy = (1, 3, 'a')
    val password = "abcde"
    assert(isCompliantV2(password, policy))
  }

  it should "yield a fail result according to spec 2 (a)" in {
    Given("a non-policy-compliant")
    val policy = (1, 3, 'b')
    val password = "cdefg"
    assert(!isCompliantV2(password, policy))
  }

  it should "yield a fail result according to spec 2 (b)" in {
    Given("a non-policy-compliant password")
    val policy = (2, 9, 'c')
    val password = "ccccccccc"
    assert(!isCompliantV2(password, policy))
  }
  

}
