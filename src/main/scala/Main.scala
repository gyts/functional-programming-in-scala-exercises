package main.scala

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def formatResult(f: Int => Int, n: Int, formatString: String) =
    formatString.format(n, f(n))


  def main(args: Array[String]): Unit = {
    println(formatResult(abs, -42, "The absolute value of %d is %d."))
    println(formatResult(factorial, 5, "The factorial of %d is %d."))
    println(formatResult(fib, 42, "The %d'th fibonacci number is %d."))
    println(isSorted[Int](Array(1, 2, 3, 3, 4), (a, b) => a < b))

    // What will be the result of the following match expression?
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println("x", x)


    val xs: List[Int] = List(1,2,3,4,5)
    println("dropWhile", List.dropWhile(xs)(x => x < 4))

    println("init", List.init(List(1, 2, 3, 4)))

    println("product3", List.product3(List(1.0, 2.0, 0.0, 4.0)))

    // 3.8 See what happens when you pass Nil and Cons themselves to foldRight
    println(
      List.foldRight(
        List(1,2,3),
        Nil:List[Int])(Cons(_,_)
      )
    )

    println("length", List.length(xs))
    List.foldLeft(
      List(1,2,3), 0
    )(
      _ + _
    )

    println("flatMap example", List.flatMap(List(1,2,3))(i => List(i,i)))
    println("add int lists", List.addIntListsTogether(List(1,2,3), List(4,5,6,7)))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else loop(n - 1, n * acc)

    loop(n, 1)
  }

  // 2.1 Write a recursive function to get the nth Fibonacci number
  def fib(n: Int): Int = {
    def loop(n: Int, previous: Int, current: Int): Int =
      if (n < 1) previous
      else loop(n - 1, current, previous + current)

    loop(n, 0, 1)
  }

  // 2.2 Implement isSorted, which checks whether an Array[A] is sorted
  // according to a given comparison function
  def isSorted[A](array: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= array.length - 1) true
      else if (!ordered(array(n), array(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  // 2.3 Write this implementation
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // 2.4 Implement uncurry, which reverses the transformation of curry
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5 Implement the higher-order function that composes two functions
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}