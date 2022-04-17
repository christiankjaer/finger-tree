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
}
