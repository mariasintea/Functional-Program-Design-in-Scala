package generators

import org.scalacheck.Prop.forAll

object Main extends App {
  val generator = new RandomGenerators()
  val ints = generator.integers
  println(s"Random integer: ${ints.generate}")
  val bools = generator.booleans
  println(s"Random boolean: ${bools.generate}")
  val pairg = generator.pairs(ints, ints)
  println(s"Random pair: ${pairg.generate}")

  val listg = generator.lists
  println(s"Random list: ${listg.generate}")
  val treeg = generator.trees
  println(s"Random tree: ${treeg.generate}")

  /**
   * shows a counter example - an AssertionError which gives a example in which the function is not working
   *  println(s"Test ${generator.test(generator.pairs(listg, listg)){
   *    case (xs, ys) => (xs ++ ys).length > xs.length
   *  }}")
   */

  // testing with ScalaCheck
  forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ++ l2).size
  }
}
