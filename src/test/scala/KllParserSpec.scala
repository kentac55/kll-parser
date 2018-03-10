import java.util.NoSuchElementException

import KllParser._

import scala.reflect.ClassTag

class KllParserSpec extends UnitSpec {
  "comments()" should "parse comment literal to Comment object" in {
    val goodComments = Table(
      ("i", "o"),
      ("#test", "test"),
      ("# test", "test"),
      ("###", "##"),
      ("###test", "##test")
    )
    forAll(goodComments) { (i: String, o: String) =>
      assert(parseAll(comment, i).get == Comment(o))
    }
  }

  it should "produce RuntimeException when given word doesn't start with '#'" in {
    val badComments = Table(
      "i",
      "asdf",
      "asdf#",
      "a#df"
    )
    forAll(badComments) { (i: String) =>
      assertThrows[RuntimeException](parseAll(comment, i).get)
    }
  }

  "variables()" should "parse variable formula to Variable object" in {
    val p        = ("key", "value")
    val expected = Variable(p._1, p._2)
    val goodFormulas = Table(
      ("i", "o"),
      (s"""${p._1} = ${p._2}""", expected),
      (s"""${p._1}=${p._2}""", expected),
      (s"""\"${p._1}\" = \"${p._2}\"""", expected),
      (s"""\"${p._1}\"=\"${p._2}\"""", expected),
      (s"""\"${p._1}\"=${p._2}""", expected),
      (s"""${p._1}=\"${p._2}\"""", expected)
    )
    forAll(goodFormulas) { (i: String, o: Variable) =>
      assert(parseAll(variables, i).get == o)
    }
  }

  it should "produce RuntimeException when given word is incorrect format" in {
    val badFormulas = Table(
      "i",
      "keyvalue",
      "\"key=value",
      "key=value\""
    )
    forAll(badFormulas) { (i: String) =>
      assertThrows[RuntimeException](parseAll(variables, i).get)
    }
  }

  "scanCode()" should "parse given string to ScanCode object" in {
    val goodScanCode = Table(
      ("i", "o"),
      ("S0x2A", ScanCode(0x2A)),
      ("S42", ScanCode(42))
    )
    forAll(goodScanCode) { (i: String, o: ScanCode) =>
      assert(parseAll(scanCode, i).get == Right(o))
    }
  }

  it should "produce RuntimeException when given string is incorrect format" in {
    val badScanCode = Table(
      "i",
      "U0x2A",
      "Sasdf",
      "S-0x2A",
      "S-10"
    )
    forAll(badScanCode) { (i: String) =>
      assertThrows[RuntimeException](parseAll(scanCode, i).get)
    }
  }

  "usbCode()" should "parse given string to UsbCode Object" in {
    val goodUsbCode = Table(
      ("i", "o"),
      ("U0x2A", USBCode(0x2A)),
      ("U42", USBCode(42)),
      ("U\"A\"", USBCode(4)),
      ("U\"a\"", USBCode(4)),
      ("U\"Backspace\"", USBCode(42)),
      ("U\"backspace\"", USBCode(42)),
      ("U\"-\"", USBCode(0x2D))
    )
    forAll(goodUsbCode) { (i: String, o: USBCode) =>
      assert(parseAll(usbCode, i).get == Right(o))
    }
  }

  it should "produce RuntimeException when given format is incorrect" in {
    val badUsbCode = Table(
      "i",
      "S0x2A",
      "Uasdf",
      "U-10"
    )
    forAll(badUsbCode) { (i: String) =>
      assertThrows[RuntimeException](parseAll(usbCode, i).get)
    }
  }

  it should "produce NoSuchElementException when given string cannot be found in table" in {
    val nonExistUsbCode = Table(
      "i",
      "U\"asdf\""
    )
    forAll(nonExistUsbCode) { (i: String) =>
      assert(parseAll(usbCode, i).get match {
        case Left(e) => e.isInstanceOf[NoSuchElementException]
        case _       => false
      })
    }
  }
}
