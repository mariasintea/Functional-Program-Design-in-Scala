package generators

import java.util.Random

class RandomGenerators {
  val integers: Generator[Int] = new Generator[Int] {
    val rand = new Random
    def generate: Int = rand.nextInt()
  }

  /**
   * First implementation:
   *   val booleans = new Generator[Boolean] {
   *     def generate: Boolean = integers.generate > 0
   *   }
   */
  val booleans: Generator[Boolean] = for (x <- integers) yield x > 0

  /**
   * First implementation:
   *   val pairs = new Generator[(Int, Int)] {
   *     def generate: (Int, Int) = (integers.generate, integers.generate)
   *   }
   */
  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = t.flatMap(x => u.map(y => (x, y)))

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate: T = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi -lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  def lists: Generator[List[Int]] =
    for{
      isEmpty <- booleans
      list <- if (isEmpty) emptyLists else nonEmptyLists
    } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists =
    for {
      head <- integers
      tail <- lists
    } yield head :: tail

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if(isLeaf) leafs else inners
  } yield tree

  def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
    for (i <- 0 until numTimes){
      val value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numTimes + "tests")
  }
}
