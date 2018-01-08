package main.scala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.2 Implement the function tail for removing the first element of a List
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // 3.3 Implement the function setHead for replacing the first element
  // of a List with a different value.
  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(_, xs) => Cons(a, xs)
  }

  // 3.4 Generalize tail to the function drop,
  // which removes the first n elements from a list.
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  // 3.5 Implement dropWhile, which removes elements from the List prefix
  // as long as they match a predicate.
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t)(f)
      else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // 3.6 Implement a function, init, that returns a List
  // consisting of all but the last element of a List
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // 3.7
  def product3(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => if (x == 0.0) 0.0 else x * y)

  // 3.9 Compute the length of a list using foldRight
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  // 3.10 Write another general list-recursion function, foldLeft
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = ???
}
