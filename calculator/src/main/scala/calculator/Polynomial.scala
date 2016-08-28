package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    // Δ = b² - 4ac
    Signal(b()*b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    if (delta() <0) Set.empty
    else {
      Set( (math.sqrt(delta()) - b()) / (2 * a()), ( (-math.sqrt(delta()) - b() )/ (2 * a())))
    }
  }
}
