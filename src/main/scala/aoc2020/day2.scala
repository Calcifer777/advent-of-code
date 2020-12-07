package aoc2020

import scala.io.Source
import scala.util.matching.Regex

/* https://adventofcode.com/2020/day/2 */
object PasswordFinder extends App {

  type Policy = (Int, Int, Char)

  def parseInput(s: String, pattern: String): (Policy, String) = {
    val regex = pattern.r
    val regex(min, max, char, password) = s
    ((min.toInt, max.toInt, char.head), password)
  }

  def isCompliant(password: String, policy: Policy): Boolean = {
    val cnt = password.count(_ == policy._3)
    (policy._1 <= cnt) && (cnt <= policy._2)
  }

  val pattern = raw"(\d+)-(\d+) (\w): (\w+)"
  val xs: List[(Policy, String)] = 
    Source.fromResource("2020/day2.txt").getLines
       .map((s: String) => parseInput(s, pattern))
       .toList

  val compliantPasswordsNum = xs.count {
    case (p, pwd) => isCompliant(pwd, p)
  }
  println(s"Number of compliant passwords: ${compliantPasswordsNum}")

  def isCompliantV2(password: String, policy: Policy): Boolean = {
    (password(policy._1-1) == policy._3) ^
    (password(policy._2-1) == policy._3)    
  }

  val compliantPasswordsV2Num = xs.count {
    case (p, pwd) => isCompliantV2(pwd, p)
  }
  println(s"Number of compliant passwords according to spec 2: ${compliantPasswordsV2Num}")
  
}
