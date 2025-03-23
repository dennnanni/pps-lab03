package u03

import u02.Modules.*
import u02.Modules.Person.*
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.*
import u03.Sequences.Sequence.{Nil, *}
import u03.Streams.*
import u03.Streams.Stream.*

object Lab03 extends App:

  // Task 1
  /*
       * Skip the first n elements of the sequence
       * E.g., [10, 20, 30], 2 => [30]
       * E.g., [10, 20, 30], 3 => []
       * E.g., [10, 20, 30], 0 => [10, 20, 30]
       * E.g., [], 2 => []
       */
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(h, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  /*
   * Zip two sequences
   * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
   * E.g., [10], [] => []
   * E.g., [], [] => []
   */
  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case _ => Nil()

  /*
   * Concatenate two sequences
   * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
   * E.g., [10], [] => [10]
   * E.g., [], [] => []
   */
  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h, t) => Cons(h, concat(t, s2))
    case _ => s2

  /*
   * Reverse the sequence
   * E.g., [10, 20, 30] => [30, 20, 10]
   * E.g., [10] => [10]
   * E.g., [] => []
   */
  def reverse[A](s: Sequence[A]): Sequence[A] =
    def buildSequence(s1: Sequence[A], acc: Sequence[A]): Sequence[A] = s1 match
      case Cons(h, t) => buildSequence(t, Cons(h, acc))
      case Nil() => acc

    buildSequence(s, Nil())

  /*
   * Map the elements of the sequence to a new sequence and flatten the result
   * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
   * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
   * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
   */
  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  /*
   * Get the minimum element in the sequence
   * E.g., [30, 20, 10] => 10
   * E.g., [10, 1, 30] => 1
   */
  def min(s: Sequence[Int]): Optional[Int] = s match
    case Cons(h, t) => min(t) match
      case Just(v) if v < h => Just(v)
      case _ => Just(h)
    case _ => Empty()

  /*
   * Get the elements at even indices
   * E.g., [10, 20, 30] => [10, 30]
   * E.g., [10, 20, 30, 40] => [10, 30]
   */
  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, evenIndices(skip(t)(1)))
    case _ => Nil()

  /*
   * Check if the sequence contains the element
   * E.g., [10, 20, 30] => true if elem is 20
   * E.g., [10, 20, 30] => false if elem is 40
   */
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) => if h == elem then true else contains(t)(elem)
    case _ => false

  /*
   * Remove duplicates from the sequence
   * E.g., [10, 20, 10, 30] => [10, 20, 30]
   * E.g., [10, 20, 30] => [10, 20, 30]
   */
  def distinct[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => distinct(t) match
      case Cons(h1, t1) if h == h1 => t1
      case _ => Cons(h, t)
    case _ => Nil()

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

  def fibonacci(): Stream[Int] =
    def _fib(acc1: Int, acc2: Int): Stream[Int] =
      cons(acc1, _fib(acc2, acc1 + acc2))
    _fib(0, 1)
