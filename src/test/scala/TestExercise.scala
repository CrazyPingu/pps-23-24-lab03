import Sequences.*
import Sequences.Sequence.*
import org.junit.Assert.assertEquals
import org.junit.Test
import u02.Modules.Person
import u03.Optionals.Optional.{Empty, Just}

class TestExercise:
  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  
  
  // Task 1
  @Test def testTake(): Unit =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip(): Unit =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))
    
  
  // Task 2
  @Test def testMin(): Unit =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))
    
    
  // Task 3
  @Test def testGetCourse(): Unit =
    val l: Sequence[Person] = Cons(Person.Teacher("Gianfranco", "Materia1"), Cons(Person.Student("Mario", 2001), Cons(Person.Teacher("Giovanni", "Materia2"), Nil())))
    assertEquals(Cons("Materia1", Cons("Materia2", Nil())), getCourses(l))
    val l2: Sequence[Person] = Cons(Person.Student("Mario", 2001), Cons(Person.Teacher("Giovanni", "Materia2"), Nil()))
    assertEquals(Cons("Materia2", Nil()), getCourses(l2))
    val l3: Sequence[Person] = Cons(Person.Teacher("Gianfranco", "Materia1"), Cons(Person.Student("Mario", 2001), Nil()))
    assertEquals(Cons("Materia1", Nil()), getCourses(l3))
    val l4: Sequence[Person] = Nil()
    assertEquals(Nil(), getCourses(l4))
    val l5: Sequence[Person] = Cons(Person.Student("Gianfranco", 2001), Nil())
    assertEquals(Nil(), getCourses(l5))




