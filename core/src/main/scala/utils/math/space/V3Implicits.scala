package utils.math.space

import utils.math.Scalar

trait V3Implicits{

  implicit def tripleToV3(p: (Scalar, Scalar, Scalar)): V3 = V3(p._1, p._2, p._3)

  implicit def fToV3(f: Scalar): V3 = V3(f, f, f)

  implicit def toUnit(v: V3): UnitV3 = new UnitV3(v)

}
