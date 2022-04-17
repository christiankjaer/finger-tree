import FingerTree.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import Indexed.*
import Reduce.*
import org.scalacheck.Gen

class IndexedSuite extends ScalaCheckSuite {

  val genIdx: Gen[(Int, Int)] =
    Gen.choose(1, 10000).flatMap(l => Gen.choose(0, l - 1).map((l, _)))

  property("length") {
    forAll { (a: List[Int]) =>
      a.length == Indexed(a).length
    }
  }

  property("idx") {
    forAll(genIdx) { case (l: Int, i: Int) =>
      Indexed(List.tabulate(l)(identity)).get(i) == i
    }
  }

  property("splitAt") {
    forAll(genIdx) { case (l: Int, i: Int) =>
      val list = List.tabulate(l)(identity)
      val (a, b) = Indexed(list).splitAt(i)
      list == toList(a) ++ toList(b)
    }

  }

}
