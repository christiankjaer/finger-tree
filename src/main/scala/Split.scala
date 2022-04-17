final case class Split[F[_], A](start: F[A], elem: A, end: F[A])
object Split {

  type FingerTreeSplit[V, T] = Split[[X] =>> FingerTree[V, X], T]

  // Precondition: The split will be somewhere in the block.
  private def splitDigit[V, A](p: V => Boolean, acc: V, digit: Digit[A])(using
      mon: Monoid[V],
      meas: Measured[A, V]
  ): Split[[X] =>> Option[Digit[X]], A] = {
    digit match
      case Digit.One(a) => Split(None, a, None)
      case Digit.Two(a, b) =>
        val accA = mon.combine(acc, meas.measure(a))
        if p(accA) then Split(None, a, Some(Digit.One(b)))
        else Split(Some(Digit.One(a)), b, None)
      case Digit.Three(a, b, c) =>
        val accA = mon.combine(acc, meas.measure(a))
        if p(accA) then Split(None, a, Some(Digit.Two(b, c)))
        else
          val accB = mon.combine(accA, meas.measure(b))
          if p(accB) then Split(Some(Digit.One(a)), b, Some(Digit.One(c)))
          else Split(Some(Digit.Two(a, b)), c, None)
      case Digit.Four(a, b, c, d) =>
        val accA = mon.combine(acc, meas.measure(a))
        if p(accA) then Split(None, a, Some(Digit.Three(b, c, d)))
        else
          val accB = mon.combine(accA, meas.measure(b))
          if p(accB) then Split(Some(Digit.One(a)), b, Some(Digit.Two(c, d)))
          else
            val accC = mon.combine(accB, meas.measure(c))
            if p(accC) then Split(Some(Digit.Two(a, b)), c, Some(Digit.One(d)))
            else Split(Some(Digit.Three(a, b, c)), d, None)
  }

  private def mayDeepL[V, A](
      a: Option[Digit[A]],
      m: FingerTree[V, Node[V, A]],
      sf: Digit[A]
  )(using Monoid[V], Measured[A, V]): FingerTree[V, A] = a match
    case Some(d) => FingerTree.deep(d, m, sf)
    case None =>
      ViewL(m) match
        case ViewL.NilL()      => FingerTree(sf)
        case ViewL.ConsL(n, r) => FingerTree.deep(n.toDigit, r, sf)

  private def mayDeepR[V, A](
      pr: Digit[A],
      m: FingerTree[V, Node[V, A]],
      a: Option[Digit[A]]
  )(using Monoid[V], Measured[A, V]): FingerTree[V, A] = a match
    case Some(d) => FingerTree.deep(pr, m, d)
    case None =>
      ViewR(m) match
        case ViewR.NilR()      => FingerTree(pr)
        case ViewR.SnocR(f, l) => FingerTree.deep(pr, f, l.toDigit)

  // Invariant: The split will be somewhere in the tree.
  def splitTree[V, A](p: V => Boolean, acc: V, tree: FingerTree[V, A])(using
      mon: Monoid[V],
      meas: Measured[A, V]
  ): FingerTreeSplit[V, A] =
    (tree: @unchecked) match
      case FingerTree.Single(v) =>
        Split(FingerTree.Empty(), v, FingerTree.Empty())
      case FingerTree.Deep(_, head, middle, last) =>
        // The measure of the block at the beginning
        val vpr = mon.combine(acc, digitM.measure(head))
        // The measure of the middle tree
        val vm = mon.combine(
          vpr,
          treeM[V, Node[V, A]].measure(middle)
        )
        // If the split is inside the first block
        if p(vpr) then
          val s = splitDigit(p, acc, head)
          Split(
            s.start.map(FingerTree.apply).getOrElse(FingerTree.Empty()),
            s.elem,
            mayDeepL(s.end, middle, last)
          )
        // If the split is inside the middle part
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
            mayDeepR(head, s.start, s2.start),
            s2.elem,
            mayDeepL(s2.end, s.end, last)
          )
        // If the split is at the end (symmetric to first case)
        else
          val s = splitDigit(p, vm, last)
          Split(
            mayDeepR(head, middle, s.start),
            s.elem,
            s.end.map(FingerTree.apply).getOrElse(FingerTree.Empty())
          )

  def split[V, A](p: V => Boolean, tree: FingerTree[V, A])(using
      mon: Monoid[V],
      meas: Measured[A, V]
  ): (FingerTree[V, A], FingerTree[V, A]) = tree match
    case FingerTree.Empty() => (FingerTree.Empty(), FingerTree.Empty())
    case t                  =>
      // Get the final measure
      val vt = treeM.measure(t)
      // Is the split inside?
      if p(vt) then
        val s = splitTree(p, mon.empty, t)
        (s.start, s.end.cons(s.elem))
      else (t, FingerTree.Empty())

}
