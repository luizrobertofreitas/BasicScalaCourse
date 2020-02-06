package com.basicscalacourse

object HighOrderAndCurryingMain extends App {

  def nTimesBetter(f: Int => Int, n: Int): (Int => Int) =
    if (n <= 0) (x: Int) => x
    else (x: Int) => nTimesBetter(f, n-1)(f(x))

  val plusOne: Int => Int = _ + 1

  val result = nTimesBetter(plusOne, 10)

  println(result(1))

  val superAdder: (Int) => (Int => Int) = (x: Int) => (y: Int) => x + y
  val add3 = superAdder(3)
  println(add3(4))

  // or
  println(superAdder(2)(3))

  def curriedFormatter(c: String)(x: Double): String = c.format(x)

  val standardFormatter: (Double => String) = curriedFormatter("%4.2f")
  val preciseFormatter: (Double => String) = curriedFormatter("%4.9f")

  println(standardFormatter(Math.PI)) // here, standardFormatter passes double for the curried curriedFormatter function after call it passing string format
  println(preciseFormatter(Math.PI))

  def toCurry(f: (Int, Int) => Int): (Int => Int => Int) =
    x => y => f(x, y)

  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x, y) => f(x)(y)

  def compose(f: Int => Int, g: Int => Int): Int => Int = // or def compose[A,B,T](f: A => B, g: T => A): T => B =
    x => g(f(x))

  def andThen(f: Int => Int, g: Int => Int): Int => Int = // or def andThen[A,B,C](f: A => B, g: B => C): A => C =
    x => g(f(x))

  def superAdder2: (Int => Int => Int) = toCurry(_ + _)
  def add4 = superAdder2(4)
  println(add4(17))

  val simpleAdder = fromCurry(superAdder)
  println(simpleAdder(4, 17))

  val add2 = (x: Int) => x + 2
  val times3 = (x: Int) => x * 3

  val composed = compose(add2, times3)
  val ordered = andThen(add2, times3)

  println(composed(4))
  println(ordered(4))
}
