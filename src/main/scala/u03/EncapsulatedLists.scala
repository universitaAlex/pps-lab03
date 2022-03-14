package u03

object EncapsulatedLists extends App :

  // A generic linkedlist with encapsulated state
  enum List[E]:
    private case Cons(head: E, tail: List[E])
    private case Nil()

  // a companion object (i.e., module) for List: cases are visible
  object List:

    def cons[E](head: E, tail: List[E]): List[E] = Cons(head, tail)

    def nil[E](): List[E] = Nil[E]()

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

  import List.*

  val l = cons(10, cons(20, cons(30, nil())))
  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
