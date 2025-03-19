package u03

import org.junit.*
import org.junit.Assert.*
import u02.Modules.*
import u02.Modules.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Lab03.*

class Lab03Test:

  val people =
    Cons(Teacher("piero", "informatica"),
      Cons(Student("matteo", 2024),
        Cons(Teacher("aldo", "italiano"), Nil())))
  val students =
    Cons(Student("matteo", 2024),
      Cons(Student("ilaria", 2022), Nil()))
  val empty: Sequence[Person] = Nil()

  @Test
  def testGetCoursesFromTeachers(): Unit =
    assertEquals(Cons("informatica", Cons("italiano", Nil())), getCourses(people))
    assertEquals(Nil(), getCourses(students))
    assertEquals(Nil(), getCourses(empty))