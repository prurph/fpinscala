/**
 * the target is List(1, 2, 3, 4, 5)
 * case Cons(x, Cons(2, Cons(4, _))) does not match because it skips the 3
 * case Nil does not match
 * case Cons(x, Cons(y, Cons(3, Cons(4, _)))) matches with x bound to 1 and y bound to 2, therefore the result is => x + y = 3
 */
