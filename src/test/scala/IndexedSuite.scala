import FingerTree.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import Indexed.*
import Reduce.*
import org.scalacheck.Gen

class IndexedSuite extends ScalaCheckSuite {

  property("length") {
    forAll { (a: List[Int]) =>
      a.length == Indexed(a).length
    }
  }

  property("idx") {
    forAll(Gen.choose(1, 10000)) { (a: Int) =>
      Indexed(List.tabulate(a)(_ + 1)).get(a) == a
    }
  }

}
