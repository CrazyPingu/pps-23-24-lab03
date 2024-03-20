package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u02.Modules.*
import u03.extensionmethods.Sequences.Sequence
import u03.extensionmethods.Sequences.Sequence._

class GetCourseTest:

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
