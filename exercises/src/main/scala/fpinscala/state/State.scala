package fpinscala.state

import fpinscala.state.State._


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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (next, rng2) = int.apply(rng)

    (if (next < 0) -(next + 1) else next, rng2)
  }

  def double(rng: RNG): (Double, RNG) = Some(nonNegativeInt(rng))
    .map(pair => (math.abs(pair._1 / Int.MinValue.toDouble + 1), pair._2))
    .getOrElse((0, rng))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldRight((List[Int](), rng)) { (_, acc) =>
      val (next, rng2) = int(acc._2)
      (next :: acc._1, rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap(rng: RNG): (Double, RNG) = map(nonNegativeInt)(i => math.abs(i.toDouble / Int.MinValue))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val a = ra(rng)
    val b = rb(a._2)
    (f(a._1, b._1), b._2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    fs.foldRight((List[A](), rng)) { (ra, acc) =>
      val (next, rng2) = ra(acc._2)
      (next :: acc._1, rng2)
    }
  }

  def sequence_2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight((rng: RNG) => (List[A](), rng))((f, acc) => map2(f, acc)(_ :: _))


  def intsViaSeq(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (n, r2) = f(rng)
    g(n)(r2)
  }

  def nonNegativeLessThan(limit: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % limit
    if (i + (limit - 1) - mod >= 0) rng => (mod, rng) else nonNegativeLessThan(limit)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => rng => (f(a), rng))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (n, s2) = this.run(s)
      println(s"Value will be used in flatMap: $n")
      f(n).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  val hasCandies: Boolean = candies > 0

  require(locked || hasCandies)

  def insertCoin: Machine = if (hasCandies) Machine(locked = false, candies, coins + 1) else this

  def turn: Machine = if (locked) this else Machine(locked = true, candies - 1, coins)
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.foldLeft(get[Machine]) { (state, next) =>
      println(s"next: ${next}")
      next match {
        case Coin => state.map(_.insertCoin)
        case Turn => state.map(_.turn)
      }
    }.flatMap(m => unit((m.coins, m.candies)))
  }

  def main(args: Array[String]): Unit = {
    println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 5, 10))._1)
  }
}
