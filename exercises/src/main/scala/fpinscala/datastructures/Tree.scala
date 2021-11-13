package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int =  tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(reduce: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left: Tree[A], right: Tree[A]) => reduce(fold(left)(f)(reduce), fold(right)(f)(reduce))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((x, y) => 1 + (x max y))

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    println(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
    println(maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
    println(depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))


    println(size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

  }

}