import fpinscala.laziness._

val s = Stream(1,2,3)
s.take(2).toList
s.drop(2).toList