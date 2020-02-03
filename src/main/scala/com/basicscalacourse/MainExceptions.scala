package com.basicscalacourse

object MainExceptions extends App {


  def getInt(b: Boolean): Int = if (b) throw new RuntimeException("Not valid") else 42

  val tryBlock = try {
    getInt(true)
  } catch {
    case e: RuntimeException => println(e.getMessage)
  } finally {
    println("Finally")
  }

  println(tryBlock)

  // crashing the JVM
  val crashingArray = Array.ofDim(Int.MaxValue)
}
