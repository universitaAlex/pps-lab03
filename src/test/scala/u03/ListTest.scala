package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:

  import List.*
  import Person.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l)(1))
    assertEquals(Cons(30, Nil()), drop(l)(2))
    assertEquals(Nil(), drop(l)(5))

  @Test def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l)(tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMapWithFlapMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l)(_ + ""))

  @Test def testFilterWithFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))

  @Test def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None, max(Nil()))

  @Test def testMapPersonInCourses1() =
    val list: List[Person] = Cons(Teacher("Viroli", "PPS"), Cons(Student("Baiardi", 1), Cons(Teacher("Ricci", "PCD"), Nil())))
    assertEquals(Cons("PPS", Cons("PCD", Nil())),mapPersonInCourses1(list))
    assertEquals(Nil(),mapPersonInCourses1(Cons(Student("Baiardi", 1),Nil())))
    assertEquals(Nil(),mapPersonInCourses1(Nil()))

  @Test def testMapPersonInCourses2() =
    val list: List[Person] = Cons(Teacher("Viroli", "PPS"), Cons(Student("Baiardi", 1), Cons(Teacher("Ricci", "PCD"), Nil())))
    assertEquals(Cons("PPS", Cons("PCD", Nil())),mapPersonInCourses2(list))
    assertEquals(Nil(),mapPersonInCourses2(Cons(Student("Baiardi", 1),Nil())))
    assertEquals(Nil(),mapPersonInCourses2(Nil()))

  @Test def testFold()=
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(10, foldLeft(Nil[Double]())(10)((a: Int, b: Double) => (a - b).asInstanceOf[Int]))
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(10, foldRight(Nil[Double]())(10)((a: Double, b: Int) => (a - b).asInstanceOf[Int]))