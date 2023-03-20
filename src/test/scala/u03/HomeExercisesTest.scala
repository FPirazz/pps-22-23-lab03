package u03

import org.junit.*
import org.junit.Assert.*
import org.junit.Test

class HomeExercisesTest:
  //Exercise 1
  //Point a

  @Test
  def testDropList() =
    import HomeExercises.List.*
    val lst = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(20, Cons(30, Nil ())), drop(lst, 1)) // Cons (20 , Cons (30 , Nil ()))
    assertEquals(Cons(30, Nil()), drop(lst, 2)) // Cons (30 , Nil ())
    assertEquals(Nil(), drop(lst, 5)) // Nil ()






