package com.basicscalacourse

abstract class MyList[+A] { // Whatever the subtypes informed in the methods
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](n: B): MyList[B] // Anything it receives to concat with `this`, will turn into MyList[this], because, generic tells that anything restricted to B that behaves as B extends A
  def map[B](transformer: (A)=> B): MyList[B]
  def filter(predicate: (A) => Boolean): MyList[A]
  def flatMap[B](transformer: (A) => MyList[B]): MyList[B]
  def ++[B >: A](list: MyList[B]): MyList[B] // Anything it receives to concat with `this`, will turn into MyList[this], because, generic tells that anything restricted to B that behaves as B extends A
  def printElement: String
  override def toString: String = "[" + printElement + "]"
}

case object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](n: B): MyList[B] = new Cons(n, Empty)
  override def map[B](transformer: (Nothing) => B): MyList[B] = Empty
  override def filter(predicate: (Nothing) => Boolean): MyList[Nothing] = Empty
  override def flatMap[B](transformer: (Nothing) => MyList[B]): MyList[B] = Empty
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
  override def printElement: String = ""
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h
  override def isEmpty: Boolean = false
  override def tail: MyList[A] = t
  override def add[B >: A](n: B): MyList[B] = new Cons(h, t ++ new Cons(n, Empty))
  def printElement: String = if (t.isEmpty) "" + h else h + ", " + t.printElement
  def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h, t ++ list)
  override def filter(predicate: (A) => Boolean): MyList[A] =
    if (predicate(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  override def map[B](transformer: (A) => B): MyList[B] =
    new Cons(transformer(h), t.map(transformer))
  override def flatMap[B](transformer: (A) => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)
}


object MainLists extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val list2 = new Cons(4, new Cons(5, new Cons(6, Empty)))
  val list3 = list ++ list2
  println(list3.toString)
  val list4 = list3.flatMap((a: Int) => new Cons(a, new Cons(a + 1, Empty)))
  println(list4.toString)
  val list5 = list4.map((a: Int) => new Cons(a * 2, new Cons(a * 3, Empty)))
  println(list5)

  val filteredList = list4.filter((t: Int) => t >= 5)
  println(filteredList)

  val addSomeElementsToList1 = list.add(4).add(5).add(6).add(7)
  println(addSomeElementsToList1.toString)
}
