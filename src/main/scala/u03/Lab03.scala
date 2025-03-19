package u03

import u02.Modules.*
import u02.Modules.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

object Lab03 extends App:

  def getCourses(seq: Sequence[Person]) : Sequence[String] = seq match
    case Cons(h, t) => h match
      case Teacher(_, course) => Cons(course, getCourses(t))
      case _ => getCourses(t)
    case _ => Nil()

