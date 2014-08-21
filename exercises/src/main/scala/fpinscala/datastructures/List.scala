package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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
    else Cons(as.head, apply(as.tail: _*)) // _* type annotation allows passing of a Seq to a variadic method (not unlike a splatted array?)

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
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("no tail of empty list")
      case Cons(x,xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("can't setHead of empty list")
    case Cons(_, t) => Cons(h, t) // this could also be case _ => Cons(h, List.tail(l))
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1) // or case _ => drop(List.tail(l), n -1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      // don't need a case Nil because in that case it won't match Cons(h, t) and will just return the list
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("can't init empty list")
      // returning Nil here means ultimately that the last element (which is Cons(h, Nil)) gets dropped
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  // Exercise 10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // Exercise 11
  def sumWithFoldLeft(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def productWithFoldLeft(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def lengthWithFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  // Exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  // Exercise 13
  def foldLeftWithFoldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

/*  def foldRightWithFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
      I'm not sure how this solution actually works
      foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
 */

  // Exercise 14
  def appendWithFoldRight[A](l: List[A], l2: List[A]): List[A] =
    foldRight(l, l2)(Cons(_,_))

  // Exercise 15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  // Exercise 16
  def plusOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  // Exercise 17
  def listToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((d,t) => Cons(d.toString,t))

  // Exercise 18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  // Exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    // fold right into an empty list, if the predicate is true for the head then include it
    // otherwise keep folding along with the tail
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  // Exercise 20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    // map(l)(f) returns a nested list from applying f: A => List[B] to each A in l
    // then concat flattens this
    concat(map(l)(f))

  // Exercise 21
  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    // flatMap calls concat which calls append which drops Nil entries
    flatMap(l)((a) => if (f(a)) List(a) else Nil)

  // Exercise 22
  def pairwiseListAdd[A](l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {
    // these will terminate the list when either list argument runs out of elements
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, pairwiseListAdd(t1,t2))
  }

  // Exercise 23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1,l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  // Exercise 24
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons(h,t) => {
      if (startsWith(l, sub)) true
      else hasSubsequence(t, sub)
    }
  }

  def startsWith[A](l: List[A], toMatch: List[A]): Boolean = (l, toMatch) match {
    // if we're out of stuff to match they match
    case (_, Nil) => true
    case (Cons(h1,t1), Cons(h2,t2)) if (h1 == h2) => startsWith(t1,t2)
    // otherwise they don't match
    case _ => false
  }


}