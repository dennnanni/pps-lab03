package u03

import u02.Modules.*
import u02.Modules.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Streams.*
import u03.Streams.Stream.*

object Lab03 extends App:


  // Task 2
  def getCourses(seq: Sequence[Person]): Sequence[String] =
    flatMap(seq)(_ match {case Teacher(_, c) => Cons(c, Nil()); case _ => Nil()})

  def foldLeft[A, B](seq: Sequence[A])(startValue: B)(op: (B, A) => B): B = seq match
    case Cons(h, t) => foldLeft(t)(op(startValue, h))(op)
    case _ => startValue

  def getCoursesCount(seq: Sequence[Person]): Int =
    foldLeft(Sequence.map(getCourses(seq))(_ => 1))(0)(_ + _)


  // Task 3
  // takeWhile già implementata nel file Streams

  def fill[A](n: Int)(value: A) : Stream[A] = if n > 0 then cons(value, fill(n - 1)(value)) else empty()

