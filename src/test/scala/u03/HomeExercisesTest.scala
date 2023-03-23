package u03

import org.junit.*
import org.junit.Assert.*
import org.junit.Test

class HomeExercisesTest:
  //Exercise 1
  //Point a
  import HomeExercises.List.*
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

  //Exercise 2
  @Test
  def testMax() =
    import HomeExercises.max
    import HomeExercises.Option.*
    assertEquals(Some (25), max(Cons(10, Cons(25, Cons(20, Nil()))))) // Some(25)
    assertEquals(None(), max(Nil())) // None()

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