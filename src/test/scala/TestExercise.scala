import Sequences.*
import Sequences.Sequence.*
import org.junit.Assert.assertEquals
import org.junit.Test
import u02.Modules.Person
import u03.Optionals.Optional.{Empty, Just}
import Streams.Stream
import org.junit.jupiter.api.Assertions.assertAll

class TestExercise:
  // Task 1
  @Test def testTake(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip(): Unit =
    val l1: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l1, l2))
    assertEquals(Nil(), zip(l1, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat(): Unit =
    val l1: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l1, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testFlatMap(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin(): Unit =
    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))


  // Task 2
  @Test def testGetCourse(): Unit =
    val l1: Sequence[Person] = Cons(Person.Teacher("Gianfranco", "Materia1"), Cons(Person.Student("Mario", 2001), Cons(Person.Teacher("Giovanni", "Materia2"), Nil())))
    assertEquals(Cons("Materia1", Cons("Materia2", Nil())), getCourses(l1))
    val l2: Sequence[Person] = Cons(Person.Student("Mario", 2001), Cons(Person.Teacher("Giovanni", "Materia2"), Nil()))
    assertEquals(Cons("Materia2", Nil()), getCourses(l2))
    val l3: Sequence[Person] = Cons(Person.Teacher("Gianfranco", "Materia1"), Cons(Person.Student("Mario", 2001), Nil()))
    assertEquals(Cons("Materia1", Nil()), getCourses(l3))
    val l4: Sequence[Person] = Nil()
    assertEquals(Nil(), getCourses(l4))
    val l5: Sequence[Person] = Cons(Person.Student("Gianfranco", 2001), Nil())
    assertEquals(Nil(), getCourses(l5))

  @Test def testFoldLeft(): Unit =
    val l1: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    val l2: Sequence[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(60, foldLeft(l1)(0)(_ + _))
    assertEquals(6, foldLeft(Cons(1, Cons(2, Cons(3, Nil()))))(0)(_ + _))
    assertEquals(-16, foldLeft(l2)(0)(_ - _))

  // Task 3
  @Test def testTakeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.takeWhile(str1)(_ < 6)
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil())))))), Stream.toList(str2))
    val str3 = Stream.iterate(0)(_ + 1)
    val str4 = Stream.takeWhile(str3)(_ < 0)
    assertEquals(Nil(), Stream.toList(str4))

  @Test def testFill(): Unit =
    val str1 = Stream.fill(5, "test")
    assertEquals(Cons("test", Cons("test", Cons("test", Cons("test", Cons("test", Nil()))))), Stream.toList(str1))
    val str2 = Stream.fill(0, "test")
    assertEquals(Nil(), Stream.toList(str2))
    val str3 = Stream.fill(3, 7)
    assertEquals(Cons(7, Cons(7, Cons(7, Nil()))), Stream.toList(str3))







