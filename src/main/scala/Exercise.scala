import u02.Modules.Person
import u03.Optionals.Optional
import scala.annotation.tailrec

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

    // Task 2 (Svolto da solo)
    // Es 3 ######################################
    def getCourses(l: Sequence[Person]): Sequence[String] =
      flatMap(l) {
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      }

    // Es 4 ######################################
    @tailrec
    def foldLeft[A, B](l: Sequence[A])(initial: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(initial, h))(f)
      case Nil() => initial


    // Es 5 ######################################
    extension [A](l: Sequence[A])

      def zipExtension[B](second: Sequence[B]): Sequence[(A, B)] = (l, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zipExtension(t2))
        case _ => Nil()

      def takeExtension(n: Int): Sequence[A] = (l, n) match
        case (Cons(h, t), n) if n > 0 => Cons(h, t.takeExtension(n - 1))
        case _ => Nil()

      def concatExtension(l2: Sequence[A]): Sequence[A] = l match
        case Cons(h, t) => Cons(h, t.concatExtension(l2))
        case Nil() => l2

      def flatMapExtension[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => concat(mapper(h), t.flatMapExtension(mapper))
        case Nil() => Nil()

      def getCoursesExtension: Sequence[String] = l.flatMapExtension {
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      }
      @tailrec
      def foldLeftExtension[B](initial: B)(f: (B, A) => B): B = l match
        case Cons(h, t) => t.foldLeftExtension(f(initial, h))(f)
        case Nil() => initial

object Streams:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3 (Svolto da solo)
    // Es 6 ######################################
    def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    // Es 7 ######################################
    def fill[A](n: Int, k: A): Stream[A] = n match
      case n if n > 0 => cons(k, fill(n - 1, k))
      case _ => Empty()

    // Es 8 ######################################
    


