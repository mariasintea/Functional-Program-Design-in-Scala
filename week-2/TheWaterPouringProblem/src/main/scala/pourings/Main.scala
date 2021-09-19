package pourings

object Main extends App{
  val problem = new Pouring(Vector(4, 9))
  println(s"Moves: ${problem.moves}")
  println(s"First three paths: ${problem.pathSets.take(3).toList}")
  println(s"Solutions: ${problem.solutions(6).take(3)}")
}
