package aoc2020


import scala.io.Source
import org.scalatest._
import flatspec._
import matchers._

import PasswordScanner._

class PasswordScannerSpec extends AnyFlatSpec with should.Matchers with GivenWhenThen {

  "PasswordScanner" should "pass a valid passport (1)" in {
    val passport = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
    val testFields = List(
       raw"byr:[^\s]+",
       raw"iyr:[^\s]+",
       raw"eyr:[^\s]+",
       raw"hgt:[^\s]+",
       raw"hcl:[^\s]+",
       raw"ecl:[^\s]+",
       raw"pid:[^\s]+",
      )
    assert(validate(passport, testFields))
  }

  it should "fail an invalid passport (1)" in {
    val passport = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
    val testFields = List(
       raw"byr:[^\s]+",
       raw"iyr:[^\s]+",
       raw"eyr:[^\s]+",
       raw"hgt:[^\s]+",
       raw"hcl:[^\s]+",
       raw"ecl:[^\s]+",
       raw"pid:[^\s]+",
      )
    assert(!validate(passport, testFields))
  }

  it should "pass a valid passport (2)" in {
    val passport = "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
    val testFields = List(
       raw"byr:[^\s]+",
       raw"iyr:[^\s]+",
       raw"eyr:[^\s]+",
       raw"hgt:[^\s]+",
       raw"hcl:[^\s]+",
       raw"ecl:[^\s]+",
       raw"pid:[^\s]+"
      )
    assert(validate(passport, testFields))
  }

  it should "fail an invalid passport (2)" in {
    val passport = "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"
    val testFields = List(
       raw"byr:[^\s]+",
       raw"iyr:[^\s]+",
       raw"eyr:[^\s]+",
       raw"hgt:[^\s]+",
       raw"hcl:[^\s]+",
       raw"ecl:[^\s]+",
       raw"pid:[^\s]+",
       )
    assert(!validate(passport, testFields))
  }

  it should "pass 2 passports in the sample" in {
    val raw_input: Iterator[String] = Source.fromResource("2020/day4_sample.txt").getLines
    val xs = parseInput(raw_input)
    val fieldsNoCid = List(
       raw"byr:[^\s]+",
       raw"iyr:[^\s]+",
       raw"eyr:[^\s]+",
       raw"hgt:[^\s]+",
       raw"hcl:[^\s]+",
       raw"ecl:[^\s]+",
       raw"pid:[^\s]+",
      )
    val validPasses = xs.map(validate(_, fieldsNoCid)).filter(_ == true).size
    assert(validPasses == 2)
  }

  it should "pass a valid byr" in {
    assert(byrVal("2002"))
  }

  it should "validate a passport with rules (1)" in {
    val passport = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
    val fieldsWithValidators = List(
       (raw"byr:([^\s]+)", byrVal _),
       (raw"iyr:([^\s]+)", iyrVal _),
       (raw"eyr:([^\s]+)", eyrVal _),
       (raw"hgt:([^\s]+)", hgtVal _),
       (raw"hcl:([^\s]+)", hclVal _),
       (raw"ecl:([^\s]+)", eclVal _),
       (raw"pid:([^\s]+)", pidVal _)
      )
    assert(validateWithRequirements(passport, fieldsWithValidators))
  }

  it should "fail a passport with rules (1)" in {
    val passport = "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
    val fieldsWithValidators = List(
       (raw"byr:([^\s]+)", byrVal _),
       (raw"iyr:([^\s]+)", iyrVal _),
       (raw"eyr:([^\s]+)", eyrVal _),
       (raw"hgt:([^\s]+)", hgtVal _),
       (raw"hcl:([^\s]+)", hclVal _),
       (raw"ecl:([^\s]+)", eclVal _),
       (raw"pid:([^\s]+)", pidVal _)
      )
    assert(!validateWithRequirements(passport, fieldsWithValidators))
  }



}
