package u03

import u02.Modules.*
import u02.Modules.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

object Lab03 extends App:

  def getCourses(seq: Sequence[Person]) : Sequence[String] =
    flatMap(seq)(_ match {case Teacher(_, c) => Cons(c, Nil()); case _ => Nil()})

  def foldLeft[A](seq: Sequence[A])(startValue: A)(op: (A, A) => A): A = seq match
    case Cons(h, t) => foldLeft(t)(op(startValue, h))(op)
    case _ => ???



