trait Reduce[F[_], A, B] {
  def reduceR(op: (A, B) => B)(as: F[A], b: B): B
  def reduceL(op: (B, A) => B)(b: B, as: F[A]): B
}

given listR[A, B]: Reduce[List, A, B] with {
  override def reduceR(op: (A, B) => B)(as: List[A], b: B): B =
    as.foldRight(b)(op)
  override def reduceL(op: (B, A) => B)(b: B, as: List[A]): B =
    as.foldLeft(b)(op)
}

given nodeR[V, A, B]: Reduce[[X] =>> Node[V, X], A, B] with {
  override def reduceR(op: (A, B) => B)(as: Node[V, A], z: B): B =
    as match
      case Node.Pair(_, a, b)      => op(a, op(b, z))
      case Node.Triple(_, a, b, c) => op(a, op(b, op(c, z)))

  override def reduceL(op: (B, A) => B)(z: B, as: Node[V, A]): B =
    as match
      case Node.Pair(_, b, a)      => op(op(z, b), a)
      case Node.Triple(_, c, b, a) => op(op(op(z, c), b), a)
}

given digitR[A, B]: Reduce[Digit, A, B] with {
  override def reduceR(op: (A, B) => B)(as: Digit[A], z: B): B =
    as match
      case Digit.One(a)           => op(a, z)
      case Digit.Two(a, b)        => op(a, op(b, z))
      case Digit.Three(a, b, c)   => op(a, op(b, op(c, z)))
      case Digit.Four(a, b, c, d) => op(a, op(b, op(c, op(d, z))))

  override def reduceL(op: (B, A) => B)(z: B, as: Digit[A]): B =
    as match
      case Digit.One(a)           => op(z, a)
      case Digit.Two(b, a)        => op(op(z, b), a)
      case Digit.Three(c, b, a)   => op(op(op(z, c), b), a)
      case Digit.Four(d, c, b, a) => op(op(op(op(z, d), c), b), a)
}

given treeR[V, A, B]: Reduce[[X] =>> FingerTree[V, X], A, B] with {
  override def reduceR(op: (A, B) => B)(as: FingerTree[V, A], b: B): B =
    as match
      case FingerTree.Empty()   => b
      case FingerTree.Single(v) => op(v, b)
      case FingerTree.Deep(_, head, middle, last) =>
        val op_ = digitR.reduceR(op)
        val op__ = treeR[V, Node[V, A], B].reduceR(nodeR.reduceR(op))
        op_(head, op__(middle, op_(last, b)))

  override def reduceL(op: (B, A) => B)(b: B, as: FingerTree[V, A]): B =
    as match
      case FingerTree.Empty()   => b
      case FingerTree.Single(v) => op(b, v)
      case FingerTree.Deep(_, head, middle, last) =>
        val op_ = digitR.reduceL(op)
        val op__ = treeR[V, Node[V, A], B].reduceL(nodeR.reduceL(op))
        op_(op__(op_(b, head), middle), last)

}


object Reduce {

  def toList[F[_], A](as: F[A])(using red: Reduce[F, A, List[A]]): List[A] = {
    red.reduceR(_ :: _)(as, List.empty)
  }

}