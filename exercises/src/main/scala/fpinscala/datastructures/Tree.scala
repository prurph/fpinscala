package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  // Exercise 26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // Exercise 27
  def depth[A](t: Tree[A]): Int = t match {
    // Leafs contribute size but not depth (hence 0 here but 1 in #size)
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 29
  // It is helpful to think of this as taking a functional "handler" for each of the two types of constructor: Leaf and Branch
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    // g is type (B,B) => B because we go down to the leaves, then use f: A => B so effectively when we call g it's always on
    // the return type of f which is our final result type
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeAsFold[A](t: Tree[A]): Int =
    // this is the full implementation with variables, etc.
    // fold(t)((a) => 1)((a,b) => 1 + a + b)

    // this is the super-slick way
    fold(t)(_ => 1)(1 + _ + _)

  // if you don't specify Tree[Int] this won't work because max requires numeric arguments
  def maxAsFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthAsFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((a,b) => 1 + (a max b))

  def mapAsFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    // when using fold to map the second function just takes the results from each side of the branch and reconstructs
    // the branch from it (so we're not actually "folding" so to speak)
    fold(t)(a => Leaf(f(a)): Tree[B])((b1,b2) => Branch(b1,b2))

}