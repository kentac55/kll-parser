import util.CaseInsensitiveMap

class CaseInsensitiveMapSpec extends UnitSpec {
  val lowerCaseMap        = Map(("key1", 1), ("key2", 2))
  val upperCaseMap        = Map(("KEY1", 1), ("KEY2", 2))
  val lowerInsensitiveMap = CaseInsensitiveMap(lowerCaseMap)
  val upperInsensitiveMap = CaseInsensitiveMap(upperCaseMap)

  "get()" should "return value specify by key" in {
    assert(lowerInsensitiveMap.get("key1") == Some(1))
    assert(lowerInsensitiveMap.get("KEY1") == Some(1))
    assert(upperInsensitiveMap.get("key1") == Some(1))
    assert(upperInsensitiveMap.get("KEY1") == Some(1))
  }

  "contains()" should "return the key with ignore case is contain or not" in {
    assert(lowerInsensitiveMap.contains("key1"))
    assert(lowerInsensitiveMap.contains("KEY1"))
    assert(upperInsensitiveMap.contains("key1"))
    assert(upperInsensitiveMap.contains("KEY1"))
  }

  "+()" should "add kv pair of argument" in {
    val expectedResult = CaseInsensitiveMap(Map(("key1", 1), ("key2", 2), ("add1", 3)))
    assert(lowerInsensitiveMap + (("add1", 3)) == expectedResult)
    assert(lowerInsensitiveMap + (("ADD1", 3)) == expectedResult)
    assert(upperInsensitiveMap + (("add1", 3)) == expectedResult)
    assert(upperInsensitiveMap + (("ADD1", 3)) == expectedResult)
  }

  "iterator()" should "return iterator with case insensitive" in {
    assert(lowerInsensitiveMap.iterator sameElements Map(("key1", 1), ("key2", 2)).iterator)
    assert(upperInsensitiveMap.iterator sameElements Map(("key1", 1), ("key2", 2)).iterator)
  }

  "-()" should "remove kv pair of argument" in {
    val expectedResult = CaseInsensitiveMap(Map(("key2", 2)))
    assert(lowerInsensitiveMap - "key1" == expectedResult)
    assert(upperInsensitiveMap - "key1" == expectedResult)
  }

  "factory method" should "build util.CaseInsensitiveMap object" in {
    assert(lowerInsensitiveMap.isInstanceOf[CaseInsensitiveMap[Int]])
    assert(upperInsensitiveMap.isInstanceOf[CaseInsensitiveMap[Int]])
  }
}
