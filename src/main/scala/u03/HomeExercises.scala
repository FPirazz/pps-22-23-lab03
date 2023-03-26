package u03



import scala.annotation.tailrec

object HomeExercises extends App:
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
    //Exercise 1
    //Point a, sviluppato con Nediani
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (Cons(_, t), 1) => t
      case (Cons(_, t), n) => drop(t, n-1)
    //Point b, sviluppato con Nediani
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), _) => right
      case (Cons(h, Nil()), right) => Cons(h, right)
      case (Cons(_, _), Nil()) => left
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, append(t1, Cons(h2, t2)))
    //Point c, sviluppato con Nediani
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
      case (Cons(h, Nil()), f) => f(h)
      case (Cons(h, t), f) => append(f(h), flatMap(t)(f))
    //Point d
    def mapFM[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(_, _) => flatMap(l)(h => Cons(mapper(h), Nil()))
      case Nil() => Nil()
    def filterFM[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(_, _) => flatMap(l1)(h => pred(h) match
        case true => Cons(h, Nil())
        case _ => Nil()
      )


  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])
  object Stream:
    def empty[A](): Stream[A] = Empty()
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()
    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()
    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()
    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()
    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))
    //Exercise 5
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(_, tail), 1) => tail()
      case (Cons(_, tail), n) if n > 0 => drop(tail())(n - 1)
      case _ => Empty()
    //Exercise 6
    def constant[A](const: => A): Stream[A] =
      cons(const, constant(const))

  end Stream


  //Exercise 2
  import u02.Optionals.Option
  import u02.Optionals.Option._
  import HomeExercises.List._
  def max(l: List[Int]): Option[Int] = l match
    case Nil() => None()
    case Cons(h, Nil()) => Some(h)
    case Cons(h1, Cons(h2, t2)) => if h1 > h2 then max(Cons(h1, t2)) else max(Cons(h2, t2))


  //Exercise 3
  import u02.Modules.Person._
  import u02.Modules.Person
  def courses(l: List[Person]): List[String] = l match
    case Nil() => Nil()
    case _ => List.flatMap(l)(p => p match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )


  //Exercise 4
  def foldLeft[A](l: List[A])(df: A)(f: (A, A) => A): A = l match
    case Cons(h, Nil()) => f(df, h)
    case Cons(h, t) => foldLeft(t)(f(df, h))(f)
  def foldRight[A](l: List[A])(df: A)(f: (A, A) => A): A = l match
    case Cons(h, Nil()) => f(h, df)
    case Cons(h, t) => f(h, foldRight(t)(df)(f))


  //Exercise 7
  val fibs: Stream[Int] = Stream.map(Stream.iterate((0, 1))((a, b) => (a, b) match
    case (a, b) => (b, a + b))
  )((a, _) => a)






