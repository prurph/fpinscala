import fpinscala.state._

val r = RNG.Simple(5)
val randInts = RNG.ints(5)(r)
randInts._1
randInts._2.nextInt
List.fill(4)(RNG.int)

RNG.map2(RNG.int, RNG.int)((a,b) => a*b)(r)
RNG.map2(RNG.int, RNG.int)((a,b) => a*b)(RNG.Simple(4))

val s = State(RNG.int)
val s2 = s.map2(s)((a,b) => a*b).run(RNG.Simple(4))
s.get
