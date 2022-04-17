enum Node[V, +T] {
  case Pair(m: V, a: T, b: T)
  case Triple(m: V, a: T, b: T, c: T)
  val m: V

  def toDigit: Digit[T] =
    this match
      case Pair(_, a, b)      => Digit.Two(a, b)
      case Triple(_, a, b, c) => Digit.Three(a, b, c)
}

object Node {
  def pair[V, T](a: T, b: T)(using
      mon: Monoid[V],
      meas: Measured[T, V]
  ): Node[V, T] =
    Pair(mon.combine(meas.measure(a), meas.measure(b)), a, b)
  def triple[V, T](a: T, b: T, c: T)(using
      mon: Monoid[V],
      meas: Measured[T, V]
  ): Node[V, T] =
    Triple(
      mon.combine(
        meas.measure(a),
        mon.combine(meas.measure(b), meas.measure(c))
      ),
      a,
      b,
      c
    )

}

enum Digit[+T] {
  case One(a: T)
  case Two(a: T, b: T)
  case Three(a: T, b: T, c: T)
  case Four(a: T, b: T, c: T, d: T)
}

enum FingerTree[V, +T] {
  // Empty arg case class instead of case object due to invariance in V.
  case Empty[V]() extends FingerTree[V, Nothing]
  case Single(v: T) extends FingerTree[V, T]
  case Deep(
      m: V,
      head: Digit[T],
      middle: FingerTree[V, Node[V, T]],
      last: Digit[T]
  ) extends FingerTree[V, T]

  import Digit.*
  import FingerTree.*
  import Node.*

  def cons[A >: T](
      a: A
  )(using Monoid[V], Measured[T, V], Measured[A, V]): FingerTree[V, A] =
    this match
      case Empty()   => Single(a)
      case Single(b) => deep(One(a), Empty(), One(b))
      case Deep(_, Four(b, c, d, e), middle, last) =>
        deep(Two(a, b), middle.cons(triple(c, d, e)), last)
      case Deep(_, One(b), middle, last) =>
        deep(Two(a, b), middle, last)
      case Deep(_, Two(b, c), middle, last) =>
        deep(Three(a, b, c), middle, last)
      case Deep(_, Three(b, c, d), middle, last) =>
        deep(Four(a, b, c, d), middle, last)

  def snoc[A >: T](
      a: A
  )(using Monoid[V], Measured[A, V], Measured[T, V]): FingerTree[V, A] =
    this match
      case Empty()   => Single(a)
      case Single(b) => deep(One(b), Empty[V](), One(a))
      case Deep(_, first, middle, Four(e, d, c, b)) =>
        deep(first, middle.snoc(triple(e, d, c)), Two(b, a))
      case Deep(_, first, middle, Three(d, c, b)) =>
        deep(first, middle, Four(d, c, b, a))
      case Deep(_, first, middle, Two(c, b)) =>
        deep(first, middle, Three(c, b, a))
      case Deep(_, first, middle, One(b)) =>
        deep(first, middle, Two(b, a))

  private def app3[A >: T](ts: List[A], that: FingerTree[V, A])(using
      Monoid[V],
      Measured[A, V],
      Measured[T, V]
  ): FingerTree[V, A] = {

    def nodes(as: List[A]): List[Node[V, A]] = as match
      case List(a, b)       => List(Node.pair(a, b))
      case List(a, b, c)    => List(Node.triple(a, b, c))
      case List(a, b, c, d) => List(Node.pair(a, b), Node.pair(c, d))
      case a :: b :: c :: xs =>
        Node.triple(a, b, c) :: nodes(xs)
      case _ => List.empty // Should not be possible

    (this, ts, that) match
      case (Empty(), ts, xs)   => consF(ts, xs)
      case (xs, ts, Empty())   => snocF(xs, ts)
      case (Single(x), ts, xs) => consF(ts, xs).cons(x)
      case (xs, ts, Single(x)) => snocF(xs, ts).snoc(x)
      case (Deep(_, pr1, m1, sf1), ts, Deep(_, pr2, m2, sf2)) =>
        deep(
          pr1,
          m1.app3(nodes(Reduce.toList(sf1) ++ ts ++ Reduce.toList(pr2)), m2),
          sf2
        )
  }

  def append[A >: T](
      that: FingerTree[V, A]
  )(using Monoid[V], Measured[A, V], Measured[T, V]): FingerTree[V, A] =
    this.app3(List.empty, that)

  def isEmpty(using Monoid[V], Measured[T, V]): Boolean =
    ViewL(this) match
      case ViewL.NilL()      => true
      case ViewL.ConsL(_, _) => false

  def firstElem(using Monoid[V], Measured[T, V]): Option[T] =
    ViewL(this) match
      case ViewL.NilL()         => None
      case ViewL.ConsL(head, _) => Some(head)

  def tail(using Monoid[V], Measured[T, V]): Option[FingerTree[V, T]] =
    ViewL(this) match
      case ViewL.NilL()         => None
      case ViewL.ConsL(_, tail) => Some(tail)

  def lastElem(using Monoid[V], Measured[T, V]): Option[T] =
    ViewR(this) match
      case ViewR.NilR()         => None
      case ViewR.SnocR(_, last) => Some(last)
}

object FingerTree {

  // O(1)
  def deep[V, T](
      head: Digit[T],
      middle: FingerTree[V, Node[V, T]],
      last: Digit[T]
  )(using mon: Monoid[V], meas: Measured[T, V]): FingerTree[V, T] =
    Deep(
      mon.combine(
        digitM.measure(head),
        mon.combine(treeM[V, Node[V, T]].measure(middle), digitM.measure(last))
      ),
      head,
      middle,
      last
    )

  def consF[F[_], V, A](s: F[A], t: FingerTree[V, A])(using
      meas: Measured[A, V],
      mon: Monoid[V],
      red: Reduce[F, A, FingerTree[V, A]]
  ): FingerTree[V, A] = red.reduceR((a, t) => t.cons(a))(s, t)

  def snocF[F[_], V, A](t: FingerTree[V, A], s: F[A])(using
      meas: Measured[A, V],
      mon: Monoid[V],
      red: Reduce[F, A, FingerTree[V, A]]
  ): FingerTree[V, A] = red.reduceL(_.snoc(_))(t, s)

  def apply[F[_], V, A](s: F[A])(using
      Monoid[V],
      Measured[A, V],
      Reduce[F, A, FingerTree[V, A]]
  ): FingerTree[V, A] =
    consF(s, FingerTree.Empty())
}
