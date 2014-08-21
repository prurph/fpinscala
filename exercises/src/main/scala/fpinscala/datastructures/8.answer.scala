/**
 * List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)) = Cons(1,Cons(2,Cons(3,Nil)))
 * this just returns the original list
 * think of foldRight as constructing a new List, but replacing he Nil constructor with the z argument and the Cons constructor with the function argument f
 * thus when we call foldRight with z = Nil and f = Cons(_,_) it's just like calling the List constructor
 */

