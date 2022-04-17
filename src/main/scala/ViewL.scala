enum ViewL[V, +T] {
  case NilL[V]() extends ViewL[V, Nothing]
  case ConsL(head: T, rest: FingerTree[V, T])
}

object ViewL {

  import FingerTree.*
  import Digit.*

  def apply[V, T](
      tree: FingerTree[V, T]
  )(using Monoid[V], Measured[T, V]): ViewL[V, T] = tree match
    case Empty()   => NilL()
    case Single(v) => ConsL(v, Empty())
    case Deep(_, head, middle, last) =>
      head match
        case One(a) => // Difficult part. Destruct the middle.
          val t = ViewL(middle) match
            case NilL() => FingerTree(last)
            case ConsL(head, rest) =>
              head match
                case Node.Pair(_, a, b) =>
                  deep(Two(a, b), rest, last)
                case Node.Triple(_, a, b, c) =>
                  deep(Three(a, b, c), rest, last)

          ConsL(a, t)
        case Two(a, b) =>
          ConsL(a, deep(One(b), middle, last))
        case Three(a, b, c) =>
          ConsL(a, deep(Two(b, c), middle, last))
        case Four(a, b, c, d) =>
          ConsL(a, deep(Three(b, c, d), middle, last))

}
