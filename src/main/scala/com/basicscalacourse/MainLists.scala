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

  def forEach(n: A => Unit): Unit

  def sort(f: (A, A) => Int): MyList[A]

  def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C]

  def fold[B](start: B)(operator: (B, A) => B): B
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

  override def forEach(n: Nothing => Unit): Unit = ()

  override def sort(f: (Nothing, Nothing) => Int): MyList[Nothing] = Empty

  override def zipWith[B, C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] =
    if (!list.isEmpty) throw new RuntimeException("Lists does not have the same length")
    else Empty

  override def printElement: String = ""

  override def fold[B](start: B)(operator: (B, Nothing) => B): B = start
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
    Cons(transformer(h), t.map(transformer))
  override def flatMap[B](transformer: (A) => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  override def forEach(n: A => Unit): Unit = {
    n(h)
    t.forEach(n)
  }

  override def sort(f: (A, A) => Int): MyList[A] = {
    def insert(x: A, sortedList: MyList[A]): MyList[A] =
      if (sortedList.isEmpty) Cons(x, Empty)
      else if (f(x, sortedList.head) <= 0) Cons(x, sortedList)
      else Cons(sortedList.head, insert(x, sortedList.tail))

    val sortedTail = t.sort(f)
    insert(h, sortedTail)
  }

  override def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] =
    if (list.isEmpty) throw new RuntimeException("Lists does not have the same length")
    else Cons(zip(h, list.head), t.zipWith(list.tail, zip))

  override def fold[B](start: B)(operator: (B, A) => B): B = {
//    val newStart = operator(start, h)
//    t.fold(newStart)(operator)
    t.fold(operator(start, h))(operator)
  }

}


object MainLists extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val list2 = new Cons(4, new Cons(5, new Cons(6, Empty)))
  val list3 = list ++ list2
  println(list3.toString)
  val list4 = list3.flatMap(a => new Cons(a, new Cons(a + 1, Empty)))
  println(list4.toString)
  val list5 = list4.map(a => new Cons(a * 2, new Cons(a * 3, Empty)))
  println(list5)

  val filteredList = list4.filter(_ >= 5) // equivalent to it: list4.filter(t => t >= 5)
  println(filteredList)

  val addSomeElementsToList1 = list.add(4).add(5).add(6).add(7)
  println(addSomeElementsToList1.toString)

  addSomeElementsToList1.forEach(println(_))

  println(addSomeElementsToList1.sort((x, y) => y - x))

  val listA = Cons(1, Cons(2, Empty))
  val listB = Cons(3, Cons(4, Empty))

  println(listA.zipWith[Int, String](listB, _ + " " + _))

  println(listA.fold(0)(_ + _)) // fold is a kind of reducing things
}
