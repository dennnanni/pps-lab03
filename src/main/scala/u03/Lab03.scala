package u03

import u02.Modules.*
import u02.Modules.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

object Lab03 extends App:

  def getCourses(seq: Sequence[Person]) : Sequence[String] =
    flatMap(seq)(_ match {case Teacher(_, c) => Cons(c, Nil()); case _ => Nil()})

  def foldLeft[A, B](seq: Sequence[A])(startValue: B)(op: (B, A) => B): B = seq match
    case Cons(h, t) => foldLeft(t)(op(startValue, h))(op)
    case _ => startValue

  def getCoursesCount(seq: Sequence[Person]) : Int =
    foldLeft(map(getCourses(seq))(_ => 1))(0)(_ + _)



