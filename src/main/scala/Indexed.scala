object Indexed {

  opaque type Size = Int
  opaque type Elem[T] = T

  type Indexed[T] = FingerTree[Size, Elem[T]]

  object Elem {
    def apply[T](t: T): Elem[T] = t
  }

  given Monoid[Size] with {
    override def combine(x: Size, y: Size): Int = x + y
    override def empty: Size = 0
  }

  given [T]: Measured[Elem[T], Size] with {
    override def measure(a: Elem[T]): Size = 1
  }

  def apply[T](elems: List[T]): Indexed[T] =
    FingerTree(elems)

  extension [T](seq: Indexed[T]) {
    def get(idx: Int): T =
      Split.splitTree((_: Size) > idx, 0, seq).elem

    def splitAt(idx: Int): (Indexed[T], Indexed[T]) =
      Split.split((_: Size) > idx, seq)

    def length: Int =
      treeM.measure(seq)
  }
}
