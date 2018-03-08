import util.CaseInsensitiveStringSeq

class CaseInsensitiveStringSeqSpec extends UnitSpec {
  val originalSeq  = List("abc", "xyz")
  val upperCaseSeq = CaseInsensitiveStringSeq("ABC", "XYZ")
  val lowerCaseSeq = CaseInsensitiveStringSeq("abc", "xyz")
  "contains()" should "ignore case" in {
    val UpperCaseSeq = CaseInsensitiveStringSeq("ABC", "XYZ")
    val LowerCaseSeq = CaseInsensitiveStringSeq("abc", "xyz")
    assert(UpperCaseSeq.contains("abc"))
    assert(UpperCaseSeq.contains("ABC"))
    assert(LowerCaseSeq.contains("abc"))
    assert(LowerCaseSeq.contains("ABC"))
  }
  "iterator()" should "return ignore cased iterator" in {
    assert(upperCaseSeq.iterator sameElements originalSeq.iterator)
    assert(lowerCaseSeq.iterator sameElements originalSeq.iterator)
  }
  "apply()" should "return specify indexed element" in {
    assert(upperCaseSeq(1) == "xyz")
    assert(lowerCaseSeq(1) == "xyz")
  }
  "length" should "return seq length" in {
    assert(upperCaseSeq.length == 2)
    assert(lowerCaseSeq.length == 2)
  }
  "factory method" should "build util.CaseInsensitiveStringSeq object" in {
    assert(CaseInsensitiveStringSeq("ABC", "XYZ").isInstanceOf[CaseInsensitiveStringSeq])
    assert(CaseInsensitiveStringSeq("abc", "xyz").isInstanceOf[CaseInsensitiveStringSeq])
  }
}
