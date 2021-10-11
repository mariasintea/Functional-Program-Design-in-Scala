package calculator

import scala.math.sqrt

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal({
    val bValue = b()
    bValue * bValue - 4 * a() * c()
  })

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal(
    delta() match {
      case d if d > 0 => {
        val aValue = a()
        val bValue = b()
        val sol1 = (-bValue + sqrt(d)) / (2 * aValue)
        val sol2 = (-bValue - sqrt(d)) / (2 * aValue)
        Set(sol1, sol2)
      }
      case 0 => Set(-b() / 2 * a())
      case _ => Set()
    }
  )
}
