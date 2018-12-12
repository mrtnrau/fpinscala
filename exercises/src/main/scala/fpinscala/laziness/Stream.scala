package fpinscala.laziness

import Stream._
trait Stream[+A] { self =>

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    self match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] =
    self match {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

  def toList: List[A] =
    foldRight[List[A]](Nil)(_ :: _)

  def take(n: Int): Stream[A] =
    self match {
      case Cons(h, t) if n > 1  => cons(h(), t().take(n- 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _                    => empty
    }

  def drop(n: Int): Stream[A] =
    self match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                   => self
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    self match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def takeWhile1(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](sa: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](sa)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((h, t) => f(h).append(t))

  def find1(p: A => Boolean): Option[A] =
    self.filter(p).headOption

  def map1[B](f: A => B): Stream[B] =
    unfold(self) {
      case Cons(h, t) => Some(f(h()) -> t())
      case Empty      => None
    }

  def take1(n: Int): Stream[A] =
    unfold(self -> n) {
      case (Cons(h, t), n) if n > 1 => Some(h() -> (t()   -> (n - 1)))
      case (Cons(h, _), 1)          => Some(h() -> (empty -> 0      ))
      case _                        => None
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    unfold(self) {
      case Cons(h, t) if p(h()) => Some(h() -> t())
      case _                    => None
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    unfold(self -> s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((h1() -> h2()) -> (t1() -> t2()))
      case _                            => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(self -> s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()) -> (t1() -> t2()))
      case _                            => None
    }

  def zip1[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)(_ -> _)

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(self -> s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1()     -> t2()    ))
      case (Cons(h , t ), Empty       ) => Some((Some(h() ) -> None      ) -> (t()      -> empty[B]))
      case (Empty       , Cons(h , t )) => Some((None       -> Some(h() )) -> (empty[A] -> t()     ))
      case _                            => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    self.zipAll(s).forAll {
      case (Some(a), Some(b)) => a == b
      case (None   , Some(_)) => false
      case _                  => true
    }

  def tails: Stream[Stream[A]] =
    unfold(self) {
      case Cons(h, t)     => Some(cons(h(), t()) -> t())
      case Empty          => None
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    (foldRight(z -> Stream(z)) {
      (a, bsb) => {
        lazy val bsb1 = bsb
        val b = f(a, bsb1._1)
        b -> cons(b, bsb1._2)
      }
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] =
    Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] =
    Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(fib0: Int, fib1: Int): Stream[Int] =
      Stream.cons(fib0, go(fib1, fib0 + fib1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None         => Stream.empty
    }

  val ones1: Stream[Int] =
    unfold(1)(_ => Some(1 -> 1))

  def constant1[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a -> a))

  def from1(n: Int): Stream[Int] =
    unfold(n)(s => Some(s -> (s + 1)))

  val fibs1: Stream[Int] =
    unfold(0 -> 1) { case (fib0, fib1) => Some(fib0 -> (fib1 -> (fib0 + fib1))) }
}