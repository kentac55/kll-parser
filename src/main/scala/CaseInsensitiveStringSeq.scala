import java.util.Locale

class CaseInsensitiveStringSeq private (val originalList: Seq[String])
    extends Seq[String]
    with Serializable {
  val keyLowerCasedList: Seq[String] = originalList.map(_.toLowerCase(Locale.ROOT))

  override def contains[A1 >: String](elem: A1): Boolean =
    keyLowerCasedList.contains(elem.toString.toLowerCase(Locale.ROOT))

  override def iterator: Iterator[String] = keyLowerCasedList.iterator

  override def apply(idx: Int): String = keyLowerCasedList(idx)

  override def length: Int = originalList.length

}

object CaseInsensitiveStringSeq {
  def apply(params: String*): CaseInsensitiveStringSeq = params match {
    case x: CaseInsensitiveStringSeq => x
    case _                           => new CaseInsensitiveStringSeq(params)
  }
}
