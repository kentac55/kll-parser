import org.scalatest.prop.PropertyChecks
import org.scalatest.{DiagrammedAssertions, FlatSpec}

abstract class UnitSpec extends FlatSpec with DiagrammedAssertions with PropertyChecks
