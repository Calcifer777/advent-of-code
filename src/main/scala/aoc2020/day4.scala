package aoc2020

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Try, Success, Failure}

object PasswordScanner extends App {

  def parseInput(xs: Iterator[String]): List[String] = {
    var buf = ListBuffer[String]()
    var row = ""
    xs foreach { (x: String) => 
      if (x == "") {
        buf.append(row)
        row = ""
      }
      else {
        row += " " + x
      }
    }
    buf.append(row)
    buf.toList
  }

  def validate(s: String, tokens: List[String]): Boolean = {
    tokens forall { (t: String) => t.r.findFirstIn(s) match {
        case Some(_) => true
        case None => false
      }
    }
  }

  val raw_input: Iterator[String] = Source.fromResource("2020/day4.txt").getLines

  val xs = parseInput(raw_input)
  
  val fieldsNoCid = List(
     raw"byr:(.+)\b",
     raw"iyr:(.+)\b",
     raw"eyr:(.+)\b",
     raw"hgt:(.+)\b",
     raw"hcl:(.+)\b",
     raw"ecl:(.+)\b",
     raw"pid:(.+)\b",
    )

  val validPasses = xs.map(validate(_, fieldsNoCid)).filter(_ == true).size
  println(s"# Valid Passes: $validPasses")

  def validateWithRequirement(s: String, token: String, fieldValidator: String => Boolean): Boolean = {
    token.r.findFirstMatchIn(s) match {
        case Some(tkn) => Try {fieldValidator(tkn.group(1))} match {
          case Success(v) => v
          case Failure(e) => false
        }
        case None => false
    }
  }

  def validateWithRequirements(s: String, tokens: List[(String, String => Boolean)]): Boolean = 
    tokens forall { case (tkn, validator) => validateWithRequirement(s, tkn, validator) }

  def byrVal(s: String): Boolean = (s.length == 4) && (1920 <= s.toInt) && (s.toInt <= 2002)

  def iyrVal(s: String): Boolean = (s.length == 4) && (2010 <= s.toInt) && (s.toInt <= 2020)

  def eyrVal(s: String): Boolean = (s.length == 4) && (2020 <= s.toInt) && (s.toInt <= 2030)

  def eclVal(s: String): Boolean = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains s

  def hclVal(s: String): Boolean = {
    if (!s.startsWith("#")) false
    else {
      val p = "^[0-9a-f]+$".r
      p.findFirstIn(s.tail) match {
        case Some(value) => true
        case None => false
      }
    }
  }

  def hgtVal(s: String): Boolean = {
    val (v, unit) = s.splitAt(s.length-2)
    if (unit == "cm")
      (150 <= v.toInt) && (v.toInt <= 193)
    else if (unit == "in") 
      (59 <= v.toInt) && (v.toInt <= 76)
    else 
      false
  }

  def pidVal(s: String): Boolean = {
    (s.length == 9) && (s.toList forall (_.isDigit))
  }

  val fieldsWithValidators = List(
       (raw"byr:([^\s]+)", byrVal _),
       (raw"iyr:([^\s]+)", iyrVal _),
       (raw"eyr:([^\s]+)", eyrVal _),
       (raw"hgt:([^\s]+)", hgtVal _),
       (raw"hcl:([^\s]+)", hclVal _),
       (raw"ecl:([^\s]+)", eclVal _),
       (raw"pid:([^\s]+)", pidVal _)
    )

  val validatedPasses = xs.map(validateWithRequirements(_, fieldsWithValidators)).filter(_ == true).size
  println(s"# Valid Passes with rules: $validatedPasses")
}
