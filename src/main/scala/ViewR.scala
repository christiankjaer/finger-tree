enum ViewR[V, +T] {
  case NilR[V]() extends ViewR[V, Nothing]
  case SnocR(front: FingerTree[V, T], last: T)
}
object ViewR {

  import Digit.*
  import FingerTree.*

  def apply[V, T](
      tree: FingerTree[V, T]
  )(using Monoid[V], Measured[T, V]): ViewR[V, T] = tree match
    case Empty()   => NilR()
    case Single(v) => SnocR(Empty(), v)
    case Deep(_, head, middle, last) =>
      last match
        case One(a) => // Difficult case. Destruct the middle part.
          val t = ViewR(middle) match
            case NilR() => FingerTree(head)
            case SnocR(front, last) =>
              last match
                case Node.Pair(_, b, a) =>
                  deep(head, front, Two(b, a))
                case Node.Triple(_, c, b, a) =>
                  deep(head, front, Three(c, b, a))

          SnocR(t, a)
        case Two(b, a) =>
          SnocR(deep(head, middle, One(b)), a)
        case Three(c, b, a) =>
          SnocR(deep(head, middle, Two(c, b)), a)
        case Four(d, c, b, a) =>
          SnocR(deep(head, middle, Three(d, c, b)), a)

}
