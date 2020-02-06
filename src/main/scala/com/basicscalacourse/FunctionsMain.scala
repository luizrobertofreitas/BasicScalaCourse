package com.basicscalacourse

object FunctionsMain extends App {

  val stringConcatenator = new Function2[String, String, String] {
    override def apply(v1: String, v2: String): String = v1 + v2
  }

  println(stringConcatenator("Hello ", "Mary!!"))

  def myF(x: Int): Function1[Int, Int] = new Function1[Int, Int] {
    override def apply(v1: Int): Int = x * v1
  }

  println(myF(4)(3))

  // even better:
  val specialFunction = new Function1[Int, Function1[Int, Int]] {
    override def apply(x: Int): Int => Int = new Function1[Int, Int] {
      override def apply(v1: Int): Int = x * v1
    }
  }

  println(specialFunction(2)(3))

  val specialFunctionImproved = (x: Int) => (y: Int) => x + y

  val niceAdder: (Int, Int) => Int = (a, b) => a + b

  val niceAdderEquivalence: (Int, Int) => Int = _ + _

  val nothingToTakeAndReturn: () => Unit = () => println("Nothing")

  println(niceAdder(2, 3))
  println(niceAdderEquivalence(2, 3))
  nothingToTakeAndReturn()
  println(specialFunctionImproved(12)(13))

}
