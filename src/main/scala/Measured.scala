trait Monoid[T] {
  def empty: T
  def combine(x: T, y: T): T
}

trait Measured[-A, V] {
  def measure(a: A): V
}

given nodeM[A, V](using M: Monoid[V]): Measured[Node[V, A], V] with {
  override def measure(a: Node[V, A]): V = a.m
}

given digitM[A, V](using
    mon: Monoid[V],
    meas: Measured[A, V]
): Measured[Digit[A], V] with {
  // O(1)
  override def measure(a: Digit[A]): V = a match
    case Digit.One(a)    => meas.measure(a)
    case Digit.Two(a, b) => mon.combine(meas.measure(a), meas.measure(b))
    case Digit.Three(a, b, c) =>
      mon.combine(
        meas.measure(a),
        mon.combine(meas.measure(b), meas.measure(c))
      )
    case Digit.Four(a, b, c, d) =>
      mon.combine(
        meas.measure(a),
        mon.combine(
          meas.measure(b),
          mon.combine(meas.measure(c), meas.measure(d))
        )
      )
}

given treeM[V, T](using
    mon: Monoid[V],
    meas: Measured[T, V]
): Measured[FingerTree[V, T], V] with {
  // O(1)
  override def measure(a: FingerTree[V, T]): V = a match
    case FingerTree.Empty()          => mon.empty
    case FingerTree.Single(v)        => meas.measure(v)
    case FingerTree.Deep(m, _, _, _) => m
}
