import u02.Modules.Person
import u03.Optionals.Optional

import scala.annotation.tailrec

class Exercise :
  // Task 1 (Svolto da solo)

  object Sequences:

    enum Sequence[E]:
      case Cons(head: E, tail: Sequence[E])
      case Nil()

    object Sequence:

      def sum(l: Sequence[Int]): Int = l match
        case Cons(h, t) => h + sum(t)
        case _ => 0

      def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
        case Nil() => Nil()

      def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
        case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
        case Cons(_, t) => filter(t)(pred)
        case Nil() => Nil()

      // Es 1 ######################################
      // a)
      def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
        case _ => Nil()

      // b)
      def take[A](l: Sequence[A])(n: Int): Sequence[A] = (l, n) match
        case (Cons(h, t), n) if n > 0 => Cons(h, take(t)(n - 1))
        case _ => Nil()

      // c)
      def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
        case Cons(h, t) => Cons(h, concat(t, l2))
        case Nil() => l2

      // d)
      def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case Nil() => Nil()

      // Es 2 ######################################
      def min(l: Sequence[Int]): Optional[Int] =
        @tailrec
        def loop(l: Sequence[Int], currentMin: Int): Int = l match
          case Cons(h, t) => loop(t, if (h < currentMin) h else currentMin)
          case Nil() => currentMin

        l match
          case Nil() => Optional.Empty()
          case _ => Optional.Just(loop(l, Int.MaxValue))

      // Es 3 ######################################
      def getCourses(l: Sequence[Person]): Sequence[String] =
        flatMap(l) {
          case Person.Teacher(_, c) => Cons(c, Nil())
          case _ => Nil()
        }

