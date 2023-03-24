package u03

import scala.annotation.tailrec

object HomeExercises extends App:

  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:
    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false
    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse
    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()


  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)
  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n


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


  //Exercise 2
  import HomeExercises.Option.*
  import HomeExercises.List.*
  def max(l: List[Int]): Option[Int] = l match
    case Nil() => None()
    case Cons(h, Nil()) => Some(h)
    case Cons(h1, Cons(h2, t2)) => if h1 > h2 then max(Cons(h1, t2)) else max(Cons(h2, t2))


  //Exercise 3
  import HomeExercises.Person.*
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
