package main.scala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
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
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // 3.6 Implement a function, init, that returns a List
  // consisting of all but the last element of a List
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // 3.7
  def product3(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => if (x == 0.0) 0.0 else x * y)

  // 3.9 Compute the length of a list using foldRight
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  // 3.10 Write another general list-recursion function, foldLeft
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    if (as == Nil) z
    else {
      val Cons(h, t) = as
      foldLeft(t, f(z, h))(f)
    }
  }

  // 3.11 Write sum, product, and a function to compute the length of a list using foldLeft
  def sum3(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product4(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length2[A](ns: List[A]): Int = foldRight(ns, 0)((_, acc) => acc + 1)

  // 3.12 Write a function that returns the reverse of a list.
  // See if you can write it using a fold
  def reverse[A](as: List[A]): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => Cons(a, acc))

  // 3.13 Can you write foldLeft in terms of foldRight? How about the other way around?
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a, b) => f(b, a))
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b, a) => f(a, b))

  // 3.14 Implement append in terms of either foldLeft or foldRight
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case _ => foldLeft(a1, a2)((a, b) => Cons(b, a))
    }

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case _ => foldRight(a1, a2)((b, a) => Cons(b, a))
    }

  // 3.15 Write a function that concatenates a list of lists into a single list
  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(List.append3)

  // 3.16 Write a function that transforms a list of integers by adding 1 to each element
  def add1ToEach(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // 3.17 Write a function that turns each value in a List[Double] into a String
  def toStringEachDouble(ns: List[Double]): List[String] =
    foldRight(ns, Nil: List[String])((a, b) => Cons(a.toString , b))

  // 3.18 Write a function map that generalizes modifying each element in a list
  // while maintaining the structure of the list
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // 3.19 Write a function filter that removes elements from a list
  // unless they satisfy a given predicate
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  // 3.20 Write a function flatMap that works like map except that the function
  // given will return a list instead of a single result, and that list
  // should be inserted into the final resulting list
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, b) => List.append(f(a), b))

  // 3.21 Use flatMap to implement filter
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  // 3.22 Write a function that accepts two lists and constructs a new list
  // by adding corresponding elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9)
  def addIntListsTogether(a1: List[Int], a2: List[Int]): List[Int] = ???
}
