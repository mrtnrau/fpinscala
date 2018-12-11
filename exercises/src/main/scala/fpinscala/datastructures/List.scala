package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, t) => t
      case Nil        => throw new Error("List.tail(Nil)")
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Cons(_, t) => Cons(h, t)
      case Nil        => throw new Error("List.setHead(Nil, _)")
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(_, t) => if (n <= 0) l else drop(t, n - 1)
      case Nil        => Nil
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
      case Nil          => throw new Error("List.init(Nil)")
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => b + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
      case Nil        => z
    }

  def sum3(li: List[Int]): Int =
    foldLeft(li, 0)(_ + _)

  def product3(ld: List[Double]): Double =
    foldLeft(ld, 0.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((b, _) => b + 1)

  def reverse[A](la: List[A]): List[A] =
    foldLeft(la, List[A]())((la, a) => Cons(a, la))

  def foldLeft1[A, B](la: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(la), z)((a, b) => f(b, a))

  def foldRight1[A, B](la: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(la), z)((b, a) => f(a, b))

  def append1[A](la1: List[A], la2: List[A]): List[A] =
    foldRight(la1, la2)(Cons(_, _))

  def concat[A](lla: List[List[A]]): List[A] =
    foldRight(lla, List[A]())(append(_, _))

  def mapPlusOne(li: List[Int]): List[Int] =
    li match {
      case Cons(h, t) => Cons(h + 1, mapPlusOne(t))
      case Nil        => Nil
    }

  def mapToString(ld: List[Double]): List[String] =
    ld match {
      case Cons(h, t) => Cons(h.toString, mapToString(t))
      case Nil        => Nil
    }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Cons(h, t) => Cons(f(h), map(t)(f))
      case Nil        => Nil
    }

  def filter[A](la: List[A])(f: A => Boolean): List[A] =
    la match {
      case Cons(h, t) if !f(h) => filter(t)(f)
      case _                   => la
    }

  def onlyEven(la: List[Int]): List[Int] =
    filter(la)(_ % 2 == 0)

  def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] =
    foldRight(la, List[B]())((a, b) => append(f(a), b))

  def filter1[A](la: List[A])(f: A => Boolean): List[A] =
    flatMap(la)(a => if (f(a)) List(a) else Nil)

  def zipAdd(li1: List[Int], li2: List[Int]): List[Int] =
    (li1, li2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
      case _                            => Nil
    }

  def zipWith[A, B, C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] =
    (la, lb) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
      case _                            => Nil
    }

  def startsWith[A](la: List[A], prefix: List[A]): Boolean =
    (la, prefix) match {
      case (Cons(ha, ta), Cons(hp, tp)) if ha == hp => startsWith(ta, tp)
      case (_           , Nil         )             => true
      case _                                        => false
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Cons(h, t) => startsWith(sup, sub) || hasSubsequence(t, sub)
      case Nil        => sub == Nil
    }
}
