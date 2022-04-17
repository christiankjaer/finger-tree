import FingerTree.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import Indexed.*
import Reduce.*
import org.scalacheck.Gen

class MaxHeapSuite extends ScalaCheckSuite {

  property("extractMax") {
    forAll { (as: List[Int]) =>
      as.isEmpty || MaxHeap(as).extractMax._1 == as.max
    }
  }

}
