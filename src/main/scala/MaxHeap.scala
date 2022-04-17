import scala.math.Ordering.Implicits.*
object MaxHeap {

  opaque type Prio[T] = Option[T]
  opaque type Elem[T] = T
  type MaxHeap[T] = FingerTree[Prio[T], Elem[T]]

  given [T](using O: Ordering[T]): Monoid[Prio[T]] with {
    override def empty: Prio[T] = None
    override def combine(x: Prio[T], y: Prio[T]): Prio[T] = (x, y) match
      case (None, y)          => y
      case (x, None)          => x
      case (Some(x), Some(y)) => Some(O.max(x, y))
  }

  given [T]: Measured[T, Prio[T]] with {
    override def measure(a: T): Prio[T] = Some(a)
  }

  def apply[T](xs: List[T])(using Ordering[T]): MaxHeap[T] = FingerTree(xs)

  extension [T](q: MaxHeap[T])(using Ordering[T]) {
    def extractMax: (T, MaxHeap[T]) = {
      val mq = treeM.measure(q)
      val s = Split.splitTree((_: Prio[T]) >= mq, None, q)
      (s.elem, s.start.append(s.end))
    }
  }

}
