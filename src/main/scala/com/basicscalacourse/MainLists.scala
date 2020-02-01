package com.basicscalacourse

trait MyPredicate[-T] {
  def test(t: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(a: A): B
}

abstract class MyList[+A] { // Whatever the subtypes informed in the methods
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](n: B): MyList[B] // Anything it receives to concat with `this`, will turn into MyList[this], because, generic tells that anything restricted to B that behaves as B extends A
  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def ++[B >: A](list: MyList[B]): MyList[B] // Anything it receives to concat with `this`, will turn into MyList[this], because, generic tells that anything restricted to B that behaves as B extends A
  def printElement: String
  override def toString: String = "[" + printElement + "]"
}

object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](n: B): MyList[B] = new Cons(n, Empty)
  override def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty
  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty
  override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
  override def printElement: String = ""
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h
  override def isEmpty: Boolean = false
  override def tail: MyList[A] = t
  override def add[B >: A](n: B): MyList[B] = new Cons(h, this.tail ++ new Cons(n, Empty))
  def printElement: String = if (t.isEmpty) "" + h else h + ", " + t.printElement
  def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h, t ++ list)
  override def filter(predicate: MyPredicate[A]): MyList[A] =
    if (predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  override def map[B](transformer: MyTransformer[A, B]): MyList[B] =
    new Cons(transformer.transform(h), t.map(transformer))
  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)
}


object MainLists extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val list2 = new Cons(4, new Cons(5, new Cons(6, Empty)))
  val list3 = list ++ list2
  println(list3.toString)
  val list4 = list3.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(a: Int): MyList[Int] = new Cons(a, new Cons(a + 1, Empty))
  })
  println(list4.toString)
  val list5 = list4.map(new MyTransformer[Int, MyList[Int]] {
    override def transform(a: Int): MyList[Int] = new Cons(a * 2, new Cons(a * 3, Empty))
  })
  println(list5)

  val filteredList = list4.filter(new MyPredicate[Int] {
    override def test(t: Int): Boolean = t >= 5
  })
  println(filteredList)

  val addSomeElementsToList1 = list.add(4).add(5).add(6).add(7)
  println(addSomeElementsToList1.toString)
}
