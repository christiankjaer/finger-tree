import FingerTree.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class FingerTreeSuite extends ScalaCheckSuite {
  property("toTree") {
    forAll((l: List[Int]) => toList(FingerTree(l)) == l)
  }

  property("appendLeftId") {
    forAll((x: List[Int]) =>
      toList(FingerTree.Empty.append(FingerTree(x))) == x
    )
  }

  property("appendRightId") {
    forAll((x: List[Int]) =>
      toList(FingerTree(x).append(FingerTree.Empty)) == x
    )
  }

  property("appendAssoc") {
    forAll { (a: List[Int], b: List[Int], c: List[Int]) =>
      val t1 = FingerTree(a)
      val t2 = FingerTree(b)
      val t3 = FingerTree(c)
      toList(t1.append(t2).append(t3)) == toList(t1.append(t2.append(t3)))
    }
  }

  property("isEmpty") {
    forAll((a: List[Int]) => a.isEmpty == FingerTree(a).isEmpty)
  }

  property("firstElem") {
    forAll((a: List[Int]) => a.headOption == FingerTree(a).firstElem)
  }

  property("lastElem") {
    forAll((a: List[Int]) => a.lastOption == FingerTree(a).lastElem)
  }
}
