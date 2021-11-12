package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = if (n > 0) l match {
    case Nil => Nil
    case Cons(h, tail) => drop(tail, n - 1)
  } else l

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, tail) => if (f(h)) dropWhile(tail, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, list) => Cons(h, init(list))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)


  /*
   foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)((y, x) => y + x)
   foldLeft(Cons(2, Cons(3, Nil)), 0 + 1)((y, x) => y + x)
   foldLeft(Cons(2, Cons(3, Nil)), 1)((y, x) => y + x)
   foldLeft(Cons(3, Nil), 1 + 2)((y, x) => y + x)
   foldLeft(Cons(3, Nil), 3)((y, x) => y + x)
   foldLeft(Nil, 3 + 3)((y, x) => y + x)
   foldLeft(Nil, 6)((y, x) => y + x)
   6
   */
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => b + 1)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((b, a) => Cons(a, b))

  def reverseRight[A](l: List[A]): List[A] = foldRight(l, List[A]())((a, b) => Cons(a, b)) // wrong

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((a, b) => f(b, a))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((a, b) => Cons(b, a))

  def concat[A](lists: List[List[A]]): List[A] = foldRight(lists, List[A]())(append2)

  def plusOne(l: List[Int]): List[Int] = foldRight(l, List[Int]())((a, b) => Cons(a+1, b))


  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((a, b) => append(f(a), b))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val size = length(sub)
    if (length(sup) < size) return false

    @tailrec
    def loop(whole: List[A], part: List[A], matchCount: Int): Boolean = (whole, part) match {
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) loop(xs, ys, matchCount+1) else loop(xs, sub, 0)
      case (_, Nil) => matchCount == size
      case (Nil, _) => false
    }

    loop(sup, sub, 0)
  }

  def main(args: Array[String]): Unit = {
    println(List.drop(List(1, 2, 3, 4), 2))
    println(List.dropWhile[Int](List(1, 2, 3, 4), x => x < 3))
    println(List.init(List(1, 2, 3, 4)))

    foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_))

    println(List.length(List(1, 2, 3, 4)))
    println(List.lengthLeft(List(1, 2, 3, 4)))

    println(List.reverse(List(1, 2, 3, 4)))
    println(List.reverseRight(List(1, 2, 3, 4)))

    println(List.append2(List(1, 2), List(3, 4)))
    println(List.concat(List(List(1, 2), List(3, 4), List(5, 6))))
    println(List.plusOne(List(1,2,3,4)))

    println(List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
    println(List.flatMap(List(1, 2, 3, 4, 5, 6))(i => List(i, i)))

    println(List.filter2(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))

    println(List.hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3,4,5)))
  }
}
