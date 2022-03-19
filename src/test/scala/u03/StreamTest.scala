package u03

import org.junit.Test
import org.junit.Assert.*
import Streams.*
import Lists.*

class StreamTest:

  import List.*

  @Test def testDrop() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))
    assertEquals(Stream.empty(),Stream.drop(s)(11))

  @Test def testConstant() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def testFibonacci()=
    val fibs: Stream[Int] = Stream.fibonacci()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(fibs)(8)))

