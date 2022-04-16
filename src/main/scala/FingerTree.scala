trait Monoid[T] {
  def empty: T
  def combine(x: T, y: T): T
}

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

def toList[F[_], A](as: F[A])(using red: Reduce[F, A, List[A]]): List[A] = {
  red.reduceR(_ :: _)(as, List.empty)
}

enum Node[+T] {
  case Pair(a: T, b: T)
  case Triple(a: T, b: T, c: T)
}

given nodeR[A, B]: Reduce[Node, A, B] with {
  override def reduceR(op: (A, B) => B)(as: Node[A], z: B): B =
    as match
      case Node.Pair(a, b)      => op(a, op(b, z))
      case Node.Triple(a, b, c) => op(a, op(b, op(c, z)))

  override def reduceL(op: (B, A) => B)(z: B, as: Node[A]): B =
    as match
      case Node.Pair(b, a)      => op(op(z, b), a)
      case Node.Triple(c, b, a) => op(op(op(z, c), b), a)
}

enum Digit[+T] {
  case One(a: T)
  case Two(a: T, b: T)
  case Three(a: T, b: T, c: T)
  case Four(a: T, b: T, c: T, d: T)
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

enum FingerTree[+T] {
  case Empty
  case Single(v: T)
  case Deep(head: Digit[T], middle: FingerTree[Node[T]], last: Digit[T])

  def cons[A >: T](a: A): FingerTree[A] = this match
    case Empty     => Single(a)
    case Single(b) => Deep(Digit.One(a), Empty, Digit.One(b))
    case Deep(Digit.Four(b, c, d, e), middle, last) =>
      Deep(Digit.Two(a, b), middle.cons(Node.Triple(c, d, e)), last)
    case Deep(Digit.One(b), middle, last) =>
      Deep(Digit.Two(a, b), middle, last)
    case Deep(Digit.Two(b, c), middle, last) =>
      Deep(Digit.Three(a, b, c), middle, last)
    case Deep(Digit.Three(b, c, d), middle, last) =>
      Deep(Digit.Four(a, b, c, d), middle, last)

  def snoc[A >: T](a: A): FingerTree[A] = this match
    case Empty     => Single(a)
    case Single(b) => Deep(Digit.One(b), Empty, Digit.One(a))
    case Deep(first, middle, Digit.Four(e, d, c, b)) =>
      Deep(first, middle.snoc(Node.Triple(e, d, c)), Digit.Two(b, a))
    case Deep(first, middle, Digit.Three(d, c, b)) =>
      Deep(first, middle, Digit.Four(d, c, b, a))
    case Deep(first, middle, Digit.Two(c, b)) =>
      Deep(first, middle, Digit.Three(c, b, a))
    case Deep(first, middle, Digit.One(b)) =>
      Deep(first, middle, Digit.Two(b, a))

  private def app3[A >: T](ts: List[A], that: FingerTree[A]): FingerTree[A] = {

    def nodes(as: List[A]): List[Node[A]] = as match
      case List(a, b)       => List(Node.Pair(a, b))
      case List(a, b, c)    => List(Node.Triple(a, b, c))
      case List(a, b, c, d) => List(Node.Pair(a, b), Node.Pair(c, d))
      case a :: b :: c :: xs =>
        Node.Triple(a, b, c) :: nodes(xs)
      case _ => List.empty // Should not be possible

    (this, ts, that) match
      case (Empty, ts, xs)     => FingerTree.consF(ts, xs)
      case (xs, ts, Empty)     => FingerTree.snocF(xs, ts)
      case (Single(x), ts, xs) => FingerTree.consF(ts, xs).cons(x)
      case (xs, ts, Single(x)) => FingerTree.snocF(xs, ts).snoc(x)
      case (Deep(pr1, m1, sf1), ts, Deep(pr2, m2, sf2)) =>
        Deep(pr1, m1.app3(nodes(toList(sf1) ++ ts ++ toList(pr2)), m2), sf2)
  }

  def append[A >: T](that: FingerTree[A]): FingerTree[A] =
    this.app3(List.empty, that)

  def isEmpty: Boolean =
    ViewL(this) match
      case ViewL.NilL()      => true
      case ViewL.ConsL(_, _) => false

  def firstElem: Option[T] =
    ViewL(this) match
      case ViewL.NilL()         => None
      case ViewL.ConsL(head, _) => Some(head)

  def tail: Option[FingerTree[T]] =
    ViewL(this) match
      case ViewL.NilL()         => None
      case ViewL.ConsL(_, tail) => Some(tail)

  def lastElem: Option[T] =
    ViewR(this) match
      case ViewR.NilR()         => None
      case ViewR.SnocR(_, last) => Some(last)
}

given treeR[A, B]: Reduce[FingerTree, A, B] with {
  override def reduceR(op: (A, B) => B)(as: FingerTree[A], b: B): B =
    as match
      case FingerTree.Empty     => b
      case FingerTree.Single(v) => op(v, b)
      case FingerTree.Deep(head, middle, last) =>
        val op_ = digitR.reduceR(op)
        val op__ = treeR.reduceR(nodeR.reduceR(op))
        op_(head, op__(middle, op_(last, b)))

  override def reduceL(op: (B, A) => B)(b: B, as: FingerTree[A]): B = as match
    case FingerTree.Empty     => b
    case FingerTree.Single(v) => op(b, v)
    case FingerTree.Deep(head, middle, last) =>
      val op_ = digitR.reduceL(op)
      val op__ = treeR.reduceL(nodeR.reduceL(op))
      op_(op__(op_(b, head), middle), last)

}

enum ViewL[F[+_], +T] {
  case NilL[F[+_]]() extends ViewL[F, Nothing]
  case ConsL(head: T, rest: F[T]) extends ViewL[F, T]
}

object ViewL {

  def apply[T](tree: FingerTree[T]): ViewL[FingerTree, T] = tree match
    case FingerTree.Empty     => NilL()
    case FingerTree.Single(v) => ConsL(v, FingerTree.Empty)
    case FingerTree.Deep(head, middle, last) =>
      head match
        case Digit.One(a) =>
          val t = ViewL.apply(middle) match
            case NilL() => FingerTree(last)
            case ConsL(head, rest) =>
              head match
                case Node.Pair(a, b) =>
                  FingerTree.Deep(Digit.Two(a, b), rest, last)
                case Node.Triple(a, b, c) =>
                  FingerTree.Deep(Digit.Three(a, b, c), rest, last)

          ConsL(a, t)
        case Digit.Two(a, b) =>
          ConsL(a, FingerTree.Deep(Digit.One(b), middle, last))
        case Digit.Three(a, b, c) =>
          ConsL(a, FingerTree.Deep(Digit.Two(b, c), middle, last))
        case Digit.Four(a, b, c, d) =>
          ConsL(a, FingerTree.Deep(Digit.Three(b, c, d), middle, last))

}

enum ViewR[F[+_], +T] {
  case NilR[F[+_]]() extends ViewR[F, Nothing]
  case SnocR(front: F[T], last: T) extends ViewR[F, T]
}
object ViewR {

  def apply[T](tree: FingerTree[T]): ViewR[FingerTree, T] = tree match
    case FingerTree.Empty     => NilR()
    case FingerTree.Single(v) => SnocR(FingerTree.Empty, v)
    case FingerTree.Deep(head, middle, last) =>
      last match
        case Digit.One(a) =>
          val t = ViewR.apply(middle) match
            case NilR() => FingerTree(head)
            case SnocR(front, last) =>
              last match
                case Node.Pair(b, a) =>
                  FingerTree.Deep(head, front, Digit.Two(b, a))
                case Node.Triple(c, b, a) =>
                  FingerTree.Deep(head, front, Digit.Three(c, b, a))

          SnocR(t, a)
        case Digit.Two(b, a) =>
          SnocR(FingerTree.Deep(head, middle, Digit.One(b)), a)
        case Digit.Three(c, b, a) =>
          SnocR(FingerTree.Deep(head, middle, Digit.Two(c, b)), a)
        case Digit.Four(d, c, b, a) =>
          SnocR(FingerTree.Deep(head, middle, Digit.Three(d, c, b)), a)

}

object FingerTree {

  def consF[F[_], A](s: F[A], t: FingerTree[A])(using
      red: Reduce[F, A, FingerTree[A]]
  ): FingerTree[A] = red.reduceR((a, t) => t.cons(a))(s, t)

  def snocF[F[_], A](t: FingerTree[A], s: F[A])(using
      red: Reduce[F, A, FingerTree[A]]
  ): FingerTree[A] = red.reduceL(_.snoc(_))(t, s)

  def apply[F[_], A](s: F[A])(using
      Reduce[F, A, FingerTree[A]]
  ): FingerTree[A] =
    consF(s, FingerTree.Empty)

}
