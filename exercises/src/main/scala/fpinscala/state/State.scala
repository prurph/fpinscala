package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
//    -i - 1 because Int.MinValue == -Int.MaxValue - 1
    case (i, next) if i < 0 => (-i - 1, next)
    case (i, next) => (i, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), next)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  def intsTailRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
//      we're done if count is 0, so just return the accumulated xs and the rng we have at the end
      if (count == 0) (xs, r)
      else {
        val (x, r1) = r.nextInt
        go(count - 1, r1, x :: xs)
      }
    }
    go(count, rng, List())
  }

  def doubleMap: Rand[Double] =
//  nonNegativeInt is a function that returns (Int, RNG) thus it matches the Rand[A] that map expects
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble + 1)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
//      I originally had this as rb(rng) but the solutions make a good point that it might have unexpected results in
//      the case ra == rb (RNG.int or something)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsUsingSeq(count: Int): Rand[List[Int]] =
//    fill a list with functions of type Rand[Int] and then pass this to sequence
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
//    return a function that returns (A, RNG)
    rng => {
      val (a, rng1) = f(rng)
//      g(a) gives a Rand[B] which is RNG => (B, RNG), so calling g(a) with (rng1) as the argument returns (B, RNG)
//      thus the overall return value is rng => g(a)(rng1)--a function of type Rand[B]
      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + n - 1 - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => unit(f(i)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
//    get the value of ra with flatMap, pass it to a function that then takes it, gets the value of rb and uses f(a,b)
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a,b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
//      contrast this with flatMap for RNG; here we use .run to get the result of f(a), which returns a function
      f(a).run(s1)
    })

//  Implementations using flatMap
  def mapWithFlatMap[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2WithFlatMap[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
//    flatMap takes f: A => State[S, B] and mapWithFlatMap takes f: B => C and returns State[S, C] so we're good
    flatMap(a => sb.mapWithFlatMap(b => f(a,b)))

  def sequence[S,A](ss: List[State[S,A]]): State[S, List[A]] =
    ss.foldRight(State.unit[S,List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] =
//    this is a bit different than RNG's .unit; we create a run function and then pass that to instatiate a State
    State(s => (a,s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
