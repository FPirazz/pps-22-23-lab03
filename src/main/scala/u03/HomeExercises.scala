package u03

object HomeExercises extends App:

  //Exercise 1
  //Point a, b

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0
    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()
    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (Cons(h, t), 1) => t
      case (Cons(h, t), n) => drop(t, n-1)
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(h, Nil()), right) => Cons(h, right)
      case (Cons(_, _), Nil()) => left
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, append(t1, Cons(h2, t2)))