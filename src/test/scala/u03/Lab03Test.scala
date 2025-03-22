package u03

import org.junit.*
import org.junit.Assert.*
import u02.Modules.*
import u02.Modules.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Streams.Stream.*
import u03.Streams.*
import u03.Lab03.*

class Lab03Test:

  val people =
    Cons(Teacher("piero", "informatica"),
      Cons(Student("matteo", 2024),
        Cons(Teacher("aldo", "italiano"),
          Cons(Teacher("giulia", "matematica"), Nil()))))
  val students =
    Cons(Student("matteo", 2024),
      Cons(Student("ilaria", 2022), Nil()))
  val empty: Sequence[Person] = Nil()

  @Test
  def testGetCoursesFromTeachers(): Unit =

    assertEquals(Cons("informatica", Cons("italiano", Cons("matematica", Nil()))), getCourses(people))
    assertEquals(Nil(), getCourses(students))
    assertEquals(Nil(), getCourses(empty))

  @Test
  def testFoldLeft(): Unit =
    val seq = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val empty = Nil[Int]()
    assertEquals(-16, foldLeft(seq)(0)(_ - _))
    assertEquals(-11, foldLeft(seq)(5)(_ - _))
    assertEquals(17, foldLeft(seq)(1)(_ + _))
    assertEquals(10, foldLeft(empty)(10)(_ * _))

  @Test
  def testGetCoursesCount(): Unit =
    assertEquals(3, getCoursesCount(people))
    assertEquals(0, getCoursesCount(students))
    assertEquals(0, getCoursesCount(empty))

  // Task 3
  // takeWhile già implementata nel file Streams

  @Test
  def testFill(): Unit =
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), toList(fill(3)("a")))

  @Test
  def testFibonacci(): Unit =
    val fibonacci: Stream[Int] = Lab03.fibonacci()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), toList(take(fibonacci)(5)))
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Cons(21, Cons(34, Nil())))))))))), toList(take(fibonacci)(10)))