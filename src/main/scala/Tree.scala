package main.scala

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25 Write a function size that counts the number of nodes (leaves and branches) in a tree
  def size[A](ts: Tree[A]): Int =
    ts match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

  // 3.26 Write a function maximum that returns the maximum element in a Tree[Int]
  def maximum(tn: Tree[Int]): Int = {
    def loop(tn: Tree[Int], z: Int): Int =
      tn match {
        case Leaf(n) => if (n > z) n else z
        case Branch(l, r) => loop(l, loop(r, z))
      }

    loop(tn, 0)
  }

  // 3.27 Write a function depth that returns the maximum path length
  // from the root of a tree to any leaf
  def depth[A](ts: Tree[A]): Int = {
    def loop[A](ts: Tree[A], z: Int): Int =
      ts match {
        case Leaf(_) => z + 1
        case Branch(l, r) => {
          val left = loop(l, z + 1)
          val right = loop(r, z + 1)
          if (left > right) left else right
        }
      }

    loop(ts, 0)
  }

  // 3.28 Write a function map, analogous to the method of the same name on List, that modifies
  // each element in a tree with a given function
  def map[A, B](ts: Tree[A])(implicit f: A => B): Tree[B] =
    ts match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l), map(r))
    }

  // 3.29 Generalize size, maximum, depth, and map, writing a new function fold that abstracts
  // over their similarities. Reimplement them in terms of this more general function
  def fold[A, B](ts: Tree[A], z: B)(implicit f: (A, B) => B): B =
    ts match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => fold(l, fold(r, z))
    }

  def size2[A](ts: Tree[A]): Int =
    fold(ts, 0)((_, b) => b + 1)

  def maximum2(tn: Tree[Int]): Int =
    fold(tn, 0)((a, b) => if (a > b) a else b)

//  def depth2[A](ts: Tree[A]): Int =
//    fold(ts, 0)((_, b) => b + 1)

//  def map2[A, B](ts: Tree[A])(implicit f: A => B): Tree[B] =
//    ts match {
//      case Leaf(a) => Leaf(f(a))
//      case Branch(l, r) => fold(ts, map2(l))
//    }
//    fold(ts, Nil: Tree[B])((a, _) => a match {
//      case Leaf(x) => Leaf(f(x))
//      case Branch(l, r) => Branch(map2(l), map2(r))
//    })
}
