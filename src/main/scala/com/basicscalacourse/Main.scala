package com.basicscalacourse

object Main extends App{
  val w = new Writer("John", "Wick", 1977)
  println(w.fullName)

  val n = new Novel("GoT", 2014, w)
  println(n.authorAge)

  val i = new Writer("John", "Wick", 1977)
  println(n.isWrittenBy(i))

  val counter = new Counter
  println(counter.inc.inc.inc.dec(2).inc(2).print) // chaining methods/functions
  counter.inc // No effect
  println(counter.print) // because I didn't change the val counter reference

  val mary = new Person("Mary", "Inception")
  println(mary ! new Person("John", "Matrix"))
  println(mary ! "The rockstar")
  println(!mary)
  println(mary likes "Matrix")
  println((+mary).age)
}

class Writer(firstName: String, surname: String, val yearOfBirth: Int) {
  def fullName: String = s"$firstName $surname"
  override def toString: String = s"$surname, $firstName"
}

class Novel(name: String, releaseYear: Int, author: Writer) {
  def authorAge: Int = releaseYear - author.yearOfBirth
  def isWrittenBy(a: Writer): Boolean = author == a
  def copy: Novel = new Novel(name, releaseYear, author)
  override def toString: String = s"$name released $releaseYear and written by: $author"
}

class Counter(val count: Int = 0) {
  def inc = new Counter(count + 1)
  def dec = new Counter(count - 1)
  def inc(n: Int): Counter =
    if (n <= 0) this
    else inc.inc(n-1)
  def dec(n: Int): Counter =
    if (n <= 0) this
    else dec.dec(n-1)
  def print: Int = count
}

class Person(val name: String, favoriteMovie: String, val age: Int = 29) {
  def likes(movie: String): Boolean = movie == favoriteMovie
  def !(person: Person): String = s"${this.name} is hanging out with ${person.name}"
  def !(nickname: String): Person = new Person(s"$name ($nickname)", favoriteMovie)
  def unary_! : String = s"$name, what the heck?"
  def unary_+ : Person = new Person(name, favoriteMovie, age + 1)
  def isAlive: Boolean = true

  def apply(): String = s"Hi, my name is $name and I like $favoriteMovie"
}