package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List[A]())((a, b) => a :: b)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](x: => Stream[B]): Stream[B] = foldRight(x)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b )

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), empty[B])))
    case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (empty[A], t2())))
    case _ => None
  }

  def startsWith[B>:A](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll(e => e._1 == e._2)

  def tails: Stream[Stream[A]] = unfold(this)(st => if (st == Empty) None else Some(st, st.drop(1))) append Stream(empty)

  def hasSubsequence[B>:A](s: Stream[B]): Boolean = tails exists (_ startsWith s)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def next(prev: Int, cur: Int): Stream[Int] = cons(prev + cur, next(cur, prev + cur))
    cons(0, cons(1, next(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map(as => cons(as._1, unfold(as._2)(f))).getOrElse(empty[A])

  def fibsViaUnfold: Stream[Int] = cons(0, unfold((0, 1))(st => Some(st._2, (st._2, st._1 + st._2))))

  def main(args: Array[String]): Unit = {
    val x = Stream(4, 5, 6)
    println(Stream(1, 2, 3).append(x).toList)

    println(Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList)

    println(fibs.take(20).toList)
    println(fibsViaUnfold.take(20).toList)

    println(Stream(1,2,3,4,5,6).startsWith(Stream(1,2,3)))
    println(Stream(1,2,3,4,5,6).startsWith(Stream(2,3,4)))
    println(Stream(1,2,3,4,5,6).startsWith(Stream(1,2,3,4,5,6,7,8)))

    println(Stream(1,2,3).tails.map(_.toList).toList)
  }
}