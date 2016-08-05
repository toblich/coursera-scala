package calculator

object Polynomial {

  type SigD = Signal[Double]

  def computeDelta(a: SigD, b: SigD, c: SigD): SigD = {
    Signal {
      val _b = b()
      _b * _b - 4 * a() * c()
    }
  }

  def computeSolutions(a: SigD, b: SigD, c: SigD, delta: SigD): Signal[Set[Double]] = {
    Signal {
      val _d = delta()
      val sqrtDelta = math.sqrt(_d)
      val denom = 2 * a()
      val minusB = -b()

      if (_d < 0) Set[Double]()
      else if (_d == 0) Set(minusB / denom)
      else Set( (minusB + sqrtDelta)/denom, (minusB - sqrtDelta)/denom )
    }
  }
}
