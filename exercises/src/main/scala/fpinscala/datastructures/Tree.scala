package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](ta: Tree[A]): Int =
    ta match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_)      => 1
    }

  def maximum(ti: Tree[Int]): Int =
    ti match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(i)      => i
    }

  def depth[A](ta: Tree[A]): Int =
    ta match {
      case Branch(l, r) => 1 + depth(l) max depth(r)
      case Leaf(_)      => 0
    }

  def map[A, B](ta: Tree[A])(f: A => B): Tree[B] =
    ta match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(a)      => Leaf(f(a))
    }

  def fold[A, B](ta: Tree[A])(f: A => B)(g: (B, B) => B): B =
    ta match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(a)      => f(a)
    }

  def size1[A](ta: Tree[A]): Int =
    fold(ta)(_ => 1)(1 + _ + _)

  def maximum1(ti: Tree[Int]): Int =
    fold(ti)(i => i)(_ max _)

  def depth1[A](ta: Tree[A]): Int =
    fold(ta)(_ => 0)(1 + _ max _)

  def map1[A, B](ta: Tree[A])(f: A => B): Tree[B] =
    fold(ta)(a => (Leaf(f(a)): Tree[B]))(Branch(_, _))
}