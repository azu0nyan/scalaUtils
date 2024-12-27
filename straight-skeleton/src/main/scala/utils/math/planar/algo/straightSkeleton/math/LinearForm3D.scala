package utils.math.planar.algo.straightSkeleton.math

import utils.math.space.V3


object LinearForm3D {
  def linePerp(s: V3, e: V3) =
    new LinearForm3D((e - s).normalize, e)
}

class LinearForm3D extends Cloneable {
  var A = .0
  var B = .0
  var C = .0
  var D = .0
  def this(l: LinearForm3D) = {
    this()
    this.A = l.A
    this.B = l.B
    this.C = l.C
    this.D = l.D
  }
  def this(normal: V3, offset: V3) = {
    this()
    this.A = normal.x
    this.B = normal.y
    this.C = normal.z
    this.D = -normal ** offset
  }
  def this(A: Double, B: Double, C: Double, D: Double) = {
    this()
    this.A = A
    this.B = B
    this.C = C
    this.D = D
  }
  def this(A: Double, B: Double, C: Double) = {
    this()
    this.A = A
    this.B = B
    this.C = C
  }
  def findD(offset: V3): Unit = {
    D = -normal ** offset
  }
  def pointDistance(point: V3) = {
    val den = Math.sqrt(A * A + B * B + C * C)
    if (den == 0.0F.toDouble) throw new Error("I'm not a plane " + this + "!")
    else {
      val num = A * point.x + B * point.y + C * point.z + D
      num / den
    }
  }
  def project(pt: V3) =
    pt + normal * (-pointDistance(pt) / normal.length)


  def collide(rayOrigin: V3, rayDirection: V3): Option[V3] =
    collide(rayOrigin, rayDirection, None)

  def collide(rayOrigin: V3, rayDirection: V3, distance: Option[Double]): Option[V3] = {
    var direction = rayDirection
    val den = A * direction.x + B * direction.y + C * direction.z
    if (den == 0.0F.toDouble)
      Option
        .when(pointDistance(rayOrigin) < 1.0E-4)(rayOrigin)
    else {
      val num = -D - A * rayOrigin.x - B * rayOrigin.y - C * rayOrigin.z
      val n = num / den
      direction = direction * n + rayOrigin
      if (n < 0.0F.toDouble) Some(direction)
      else if (distance.exists(d => d < n && d != Double.PositiveInfinity)) Some(direction)
      else Some(direction) //todo check wtf is OOB
    }
  }
  def collideToVector(other: LinearForm3D) =
    createNormalVector ^ other.createNormalVector

  def collide(other: LinearForm3D): Option[Ray3d] = {
    val spec = createNormalVector ^ other.createNormalVector

    Option.when(spec.length != 0.0F.toDouble) {
      val matrixA = new Matrix(Array[Array[Double]](Array(A, B, C), Array(other.A, other.B, other.C), Array(spec.x, spec.y, spec.z)))
      val matrixB = new Matrix(Array[Array[Double]](Array(-D), Array(-other.D), Array(0.0F.toDouble)))
      val res = matrixA.solve(matrixB)
      new Ray3d(new V3(res.get(0, 0), res.get(1, 0), res.get(2, 0)), spec)
    }
  }

  def collide(b: LinearForm3D, c: LinearForm3D) = if (!this.hasNaN && !b.hasNaN && !c.hasNaN) {
    val three = Matrix3d.make(this.A, this.B, this.C, b.A, b.B, b.C, c.A, c.B, c.C)
    val offset = new V3(-this.D, -b.D, -c.D)
    Jama.solve(three, offset)
  }
  else throw new Error

  def createNormalVector = V3(A, B, C).normalize

  override def toString = A + "," + B + "," + C + "," + D

  override def equals(obj: AnyRef) =
    if (!obj.isInstanceOf[LinearForm3D]) false
    else {
      val other = obj.asInstanceOf[LinearForm3D]
      A == other.A && B == other.B && C == other.C && D == other.D
    }
  def normal = new V3(A, B, C)
  def flipNormal(): Unit = {
    A = -A
    B = -B
    C = -C
    D = -D
  }
  override def clone = new LinearForm3D(A, B, C, D)
  def inFront(p: V3) = A * p.x + B * p.y + C * p.z + D > 0.0F.toDouble
  def hasNaN = A.isNaN || B.isNaN || C.isNaN || D.isNaN
}

