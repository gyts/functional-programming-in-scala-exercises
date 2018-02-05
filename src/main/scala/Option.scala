package main.scala

sealed trait Option[+A] {
  // 4.1 Implement all of the preceding functions on Option
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    if (this == None) ob else this

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if(f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2 Implement the variance function in terms of flatMap. If the mean of a sequence is m,
  // the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  def variance(xs: Seq[Double]): Option[Double] = {
    def f(x: Double, m: Double) = math.pow(x - m, 2)

    mean(xs)
      .flatMap(m =>
        mean(
          xs.map(x => f(x, m))
        )
      )
  }

  // 4.3 Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    if (a == None || b == None) None
    else a.flatMap(x => b.flatMap(y => Some(f(x, y))))

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // 4.4 Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    List.foldRight(as, Some(Nil) : Option[List[A]])((a, acc) => a.flatMap(x => acc.map(list => Cons(x, list))))
}
