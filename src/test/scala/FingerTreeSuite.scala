import FingerTree.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import Indexed.*
import Reduce.*

class FingerTreeSuite extends ScalaCheckSuite {

  def tree[T](l: List[T]): FingerTree[Size, Elem[T]] =
    FingerTree(l.map(Elem.apply))
  def empty[T]: FingerTree[Size, Elem[T]] = FingerTree.Empty()

  property("toTree") {
    forAll((l: List[Int]) => toList(tree(l)) == l)
  }

  property("appendLeftId") {

    forAll((x: List[Int]) =>
      toList(empty.append(tree(x))) == x
    )
  }

  property("appendRightId") {
    forAll((x: List[Int]) =>
      toList(tree(x).append(empty)) == x
    )
  }

  property("appendAssoc") {
    forAll { (a: List[Int], b: List[Int], c: List[Int]) =>
      val t1 = tree(a)
      val t2 = tree(b)
      val t3 = tree(c)
      toList(t1.append(t2).append(t3)) == toList(t1.append(t2.append(t3)))
    }
  }

  property("isEmpty") {
    forAll((a: List[Int]) => a.isEmpty == tree(a).isEmpty)
  }

  property("firstElem") {
    forAll((a: List[Int]) => a.headOption == tree(a).firstElem)
  }

  property("lastElem") {
    forAll((a: List[Int]) => a.lastOption == tree(a).lastElem)
  }

  property("snocFconsF") {
    forAll((a: List[Int]) =>
      toList(FingerTree.snocF(FingerTree.Empty(), a.map(Elem.apply))) == toList(
        FingerTree.consF(a.map(Elem.apply), FingerTree.Empty())
      )
    )
  }

  property("consHead") {
    forAll { (a: List[Int], b: Int) =>
      tree(a).cons(Elem(b)).firstElem.contains(Elem(b))
    }
  }

  property("snocLast") {
    forAll { (a: List[Int], b: Int) =>
      tree(a).snoc(Elem(b)).lastElem.contains(Elem(b))
    }
  }

  property("tailCons") {
    forAll { (a: List[Int], b: Int) =>
      tree(a).cons(Elem(b)).tail.map(toList).contains(Elem(a))
    }
  }

  property("consAppendCommute") {
    forAll { (a: List[Int], b: List[Int], c: Int) =>
      val at = tree(a)
      val bt = tree(b)
      toList(at.append(bt).cons(Elem(c))) == toList(at.cons(Elem(c)).append(bt))
    }
  }
}
