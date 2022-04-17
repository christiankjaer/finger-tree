final case class Split[F[_], A](start: F[A], elem: A, end: F[A])
object Split {

  type FingerTreeSplit[V, T] = Split[[X] =>> FingerTree[V, X], T]

  def splitDigit[V, A](p: V => Boolean, acc: V, digit: Digit[A])(using
      mon: Monoid[V],
      meas: Measured[A, V]
  ): Split[List, A] = {
    digit match
      case Digit.One(a) => Split(List.empty, a, List.empty)
      case Digit.Two(a, b) =>
        val accA = mon.combine(acc, meas.measure(a))
        if p(accA) then Split(List.empty, a, List(b))
        else Split(List(a), b, List.empty)
      case Digit.Three(a, b, c) =>
        val accA = mon.combine(acc, meas.measure(a))
        if p(accA) then Split(List.empty, a, List(b, c))
        else
          val accB = mon.combine(accA, meas.measure(b))
          if p(accB) then Split(List(a), b, List(c))
          else Split(List(a, b), c, List.empty)
      case Digit.Four(a, b, c, d) =>
        val accA = mon.combine(acc, meas.measure(a))
        if p(accA) then Split(List.empty, a, List(b, c, d))
        else
          val accB = mon.combine(accA, meas.measure(b))
          if p(accB) then Split(List(a), b, List(c, d))
          else
            val accC = mon.combine(accB, meas.measure(c))
            if p(accC) then Split(List(a, b), c, List(d))
            else Split(List(a, b, c), d, List.empty)
  }

  def deepL[V, A](
      a: List[A],
      m: FingerTree[V, Node[V, A]],
      sf: Digit[A]
  )(using Monoid[V], Measured[A, V]): FingerTree[V, A] = (a: @unchecked) match
    case Nil =>
      ViewL(m) match
        case ViewL.NilL()      => FingerTree(Reduce.toList(sf))
        case ViewL.ConsL(n, r) => FingerTree.deep(n.toDigit, r, sf)
    case List(a)       => FingerTree.deep(Digit.One(a), m, sf)
    case List(a, b)    => FingerTree.deep(Digit.Two(a, b), m, sf)
    case List(a, b, c) => FingerTree.deep(Digit.Three(a, b, c), m, sf)

  def deepR[V, A](
      pr: Digit[A],
      m: FingerTree[V, Node[V, A]],
      a: List[A]
  )(using Monoid[V], Measured[A, V]): FingerTree[V, A] = (a: @unchecked) match
    case Nil =>
      ViewR(m) match
        case ViewR.NilR()      => FingerTree(Reduce.toList(pr))
        case ViewR.SnocR(f, l) => FingerTree.deep(pr, f, l.toDigit)
    case List(a)       => FingerTree.deep(pr, m, Digit.One(a))
    case List(b, a)    => FingerTree.deep(pr, m, Digit.Two(b, a))
    case List(c, b, a) => FingerTree.deep(pr, m, Digit.Three(c, b, a))

  def splitTree[V, A](p: V => Boolean, acc: V, tree: FingerTree[V, A])(using
      mon: Monoid[V],
      meas: Measured[A, V]
  ): FingerTreeSplit[V, A] =
    (tree: @unchecked) match
      case FingerTree.Single(v) =>
        Split(FingerTree.Empty(), v, FingerTree.Empty())
      case FingerTree.Deep(_, head, middle, last) =>
        val vpr = mon.combine(acc, digitM.measure(head))
        val vm = mon.combine(
          vpr,
          treeM[V, Node[V, A]].measure(middle)
        )
        if p(vpr) then
          val s = splitDigit(p, acc, head)
          Split(FingerTree(s.start), s.elem, deepL(s.end, middle, last))
        else if p(vm) then
          val s = splitTree(p, vpr, middle)
          val s2 = splitDigit(
            p,
            mon.combine(
              vpr,
              treeM[V, Node[V, A]].measure(s.start)
            ),
            s.elem.toDigit
          )
          Split(
            deepR(head, s.start, s2.start),
            s2.elem,
            deepL(s2.end, s.end, last)
          )
        else
          val s = splitDigit(p, vm, last)
          Split(deepR(head, middle, s.start), s.elem, FingerTree(s.end))

  def split[V, A](p: V => Boolean, tree: FingerTree[V, A])(using
      mon: Monoid[V],
      meas: Measured[A, V]
  ): (FingerTree[V, A], FingerTree[V, A]) = tree match
    case FingerTree.Empty() => (FingerTree.Empty(), FingerTree.Empty())
    case t =>
      val s = splitTree(p, mon.empty, t)
      val vt = treeM.measure(t)
      if p(vt) then (s.start, s.end.cons(s.elem)) else (t, FingerTree.Empty())

}
