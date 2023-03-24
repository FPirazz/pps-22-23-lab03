package u03

import org.junit.*
import org.junit.Assert.*
import org.junit.Test

class HomeExercisesTest:
  //Exercise 1
  //Point a
  import u03.HomeExercises.List._
  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  @Test
  def testDropList() =
    assertEquals(Cons(20, Cons(30, Nil ())), drop(lst, 1)) // Cons (20, Cons (30, Nil()))
    assertEquals(Cons(30, Nil()), drop(lst, 2)) // Cons (30, Nil())
    assertEquals(Nil(), drop(lst, 5)) // Nil()

  //Point b
  @Test
  def testAppendList() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail)) // Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ()))))

  //Point c
  @Test
  def testFlatMapList() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    // Cons(11, Cons(21, Cons(31, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))
    // Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))

  //Point d
  @Test
  def testMapFM() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapFM(lst)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapFM(lst)(_ + ""))
  @Test
  def testFilterFM() =
    assertEquals(Cons(20, Cons(30, Nil())), filterFM(lst)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterFM(lst)(_ != 20))


  //Exercise 2
  @Test
  def testMax() =
    import u03.HomeExercises.max
    import u02.Optionals.Option._
    assertEquals(Some (25), max(Cons(10, Cons(25, Cons(20, Nil()))))) // Some(25)
    assertEquals(None(), max(Nil())) // None()


  //Exercise 3
  @Test
  def testCourses() =
    import u02.Modules.Person.*
    import u03.HomeExercises.courses
    assertEquals(Cons("LCMC", Cons("Cybersecurity", Nil())), courses(Cons(Teacher("Mario", "LCMC"), Cons(Student("Luca", 1), Cons(Teacher("Gabriele", "Cybersecurity"), Nil()))))) // Cons("LCMC", Cons("Cybersecurity", Nil()))
    assertEquals(Nil(), courses(Nil())) // Nil()
    assertEquals(Nil(), courses(Cons(Student("Luca", 1), Nil()))) // Nil()


  //Exercise 4
  @Test
  def testFolds() =
    import HomeExercises.foldLeft
    import HomeExercises.foldRight
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _)) // -16
    assertEquals(-15, foldLeft(lst)(1)(_ - _)) // -16
    assertEquals(-8, foldRight(lst)(0)(_ - _)) // -8
    assertEquals(-7, foldRight(lst)(1)(_ - _)) // -8


  //Exercise 5
  @Test
  def testDropStream() =
    import HomeExercises.Stream._
    val s = take(iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil ())))), toList(drop(s)(6))) // => Cons(6, Cons(7, Cons(8, Cons(9, Nil ()))))


  //Exercise 6
  @Test
  def testConstantStream() =
    import HomeExercises.Stream._
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), toList(take(constant("x"))(5))) // = > Cons(x, Cons(x, Cons(x, Cons(x, Cons(x, Nil())))))


  //Exercise 7
  @Test
  def testFibonacci() =
    import u03.HomeExercises.fibs
    import u03.HomeExercises.Stream._
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), toList(take(fibs)(8)))
    // = > Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil()))))))))
