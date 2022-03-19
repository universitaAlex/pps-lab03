package u03

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

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

    def drop[A](l: List[A])(n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t)(n - 1)
      case _ => l

    def append[A](left: List[A])(right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t)(right))
      case _ => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h))(flatMap(t)(f))
      case _ => Nil()

    def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filterWithFlatMap[A](l: List[A])(pred: A => Boolean): List[A] =
      val flatMapper: A => List[A] = v => v match
        case v if pred(v) => Cons(v, Nil())
        case _ => Nil()
      flatMap(l)(flatMapper)

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if h > (max(t) match {
        case Some(n) => n
        case _ => -Int.MinValue
      }) => Option(h)
      case Cons(h, t) => max(t)
      case _ => Option.empty

    def foldLeft[A,B](l: List[A])(acc: B)(bop: (B, A) => B): B = l match
      case Nil() => acc
      case Cons(h, t) => foldLeft(t)(bop(acc, h))(bop)

    def foldRight[A,B](l: List[A])(acc: B)(bop: (A, B) => B): B = l match
      case Nil() => acc
      case Cons(h, t) => bop(h,foldRight(t)(acc)(bop))

    import Person.*

    def mapPersonInCourses1(l:List[Person]):List[String]=
      map(filter(l)(p=>p match {case Teacher(n,c) => true case _ => false }))(t=>t match {case Teacher(n,c)=>c})

    def mapPersonInCourses2(l:List[Person]):List[String]=
      flatMap(l)(p=> p match { case Teacher(n,c)=> Cons(c,Nil()) case _=> Nil()})

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
