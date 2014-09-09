import fpinscala.datastructures._

val l = List(1, 2, 3, 1)
List.tail(l)

val nilList = Nil
// this throws an exception (no tail for an empty list)
// List.tail(nilList)
val l2 = List.setHead(l, "foo")
val l3 = List.setHead(l2, 1)

val l4 = List.drop(l, 1)
val l5 = List.drop(l, 2)
val l6 = List.drop(l, 3)
val l7 = List.drop(l, 9)

val l8 = List.dropWhile(l)(n => n <= 2)
val xs = List(1, 2, 3, 4, 5)
val ex1 = List.dropWhile(xs)(x => x < 4)

val test = List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
(test == List(1,2,3))

val len = List.length(l)
val len2 = List.length(Nil)

val sumFL = List.sumWithFoldLeft(l)
val prodFL = List.productWithFoldLeft(List(1.0, 2.0, 5.0))
val lenFL = List.lengthWithFoldLeft(List("foo", "bar", "baz"))
val lenFL2 = List.lengthWithFoldLeft(List("neat", 1.0, 10))

val rev = List.reverse(l)

val plusOne = List.plusOne(l)
val stringy = List.listToString(List(1.0, 2.0, 10.0))

val firstList = List(1,2,3)
val secondList = List(4,5,6)
List.pairwiseListAdd(firstList, secondList)

List.zipWith(firstList, secondList)(_+_)

List.hasSubsequence(List(1,2,3), List(2,3))
List.hasSubsequence(List(1,2,3), List(2))
List.hasSubsequence(List(1,2,3), List(1,2,3,4))
List.hasSubsequence(List(1,2,3), List(1,3,2))
List.hasSubsequence(List(1,2,3,1,2,3,5,1,2,3,4,1,2), List(1,2,3,4))

// Tree Exercises
val tree = new Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(5), Leaf(1)))
Tree.maximum(tree)

def branchlet[A](v1: A, v2: A) = new Branch[A](Leaf(v1), Leaf(v2))
val tree2 = new Branch(branchlet(1,1), Branch(branchlet(2,2), branchlet(5,10)))
Tree.maximum(tree2)
Tree.depth(tree2)

Tree.map(tree)(_*2)

Tree.size(tree)
Tree.sizeAsFold(tree)
Tree.depthAsFold(tree2)
