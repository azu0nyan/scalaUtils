package old

//import org.scalatest.funsuite

import org.scalatest.funsuite
import org.scalatest.funsuite.AnyFunSuite
import utils.math.space.{Quat, Rotations, Segment, Transform, V3}
import utils.math._
import utils.math.linalg.{LinearSystemSolver, Matrix, Matrix3x3, MatrixInverse}


class MathTests extends AnyFunSuite {

  test("matrix ops") {
    //    println(MatrixInverse.mullRow(4, 324.23531, 10).toMultilineString)
    //    println(MatrixInverse.swapRows(4, 9, 10).toMultilineString)
    //  println(MatrixInverse.sumRow(4, 9, 10.23, 12).toMultilineString)
    val m = Matrix3x3(
      1, 2, -2,
      -1, 1, -2,
      3, 2, 1)
    val inverse1 = MatrixInverse.inverseUnsafeGaussJordanElimination(m)
    val inverse2 = m.inverseUnsafe
    println(m.toMultilineString)
    println(inverse1.toMultilineString)
    println(inverse2.toMultilineString)
    assert((inverse1 *** m) ~== Matrix.id(3))
    assert((m *** inverse1) ~== Matrix.id(3))
    assert((inverse2 *** m) ~== Matrix.id(3))
    assert((m *** inverse2) ~== Matrix.id(3))
    //    println((m *** inverse1).toMultilineString)
    //    println((m *** inverse2).toMultilineString)
  }

  test("matrix ops2") {
    //    println(MatrixInverse.mullRow(4, 324.23531, 10).toMultilineString)
    //    println(MatrixInverse.swapRows(4, 9, 10).toMultilineString)
    //  println(MatrixInverse.sumRow(4, 9, 10.23, 12).toMultilineString)
    val m = Matrix.squareFromSeqs(IndexedSeq(
      IndexedSeq(1, 2, 3, 4, 5),
      IndexedSeq(2, 3, -4, 5, 6),
      IndexedSeq(6, 7, -8, 9, 10),
      IndexedSeq(1, 2, -2, 11, 12),
      IndexedSeq(-1, 1, -2, -23, 45)
    ));
    val inverse1 = MatrixInverse.inverseUnsafeGaussJordanElimination(m)
    println(m.toMultilineString)
    println(inverse1.toMultilineString)
    println("inv")
    println((inverse1 *** m).toMultilineString)
    println((m *** inverse1).toMultilineString)
    assert((inverse1 *** m) ~== Matrix.id(5))
    assert((m *** inverse1) ~== Matrix.id(5))
    //    println((m *** inverse1).toMultilineString)
    //    println((m *** inverse2).toMultilineString)
  }

  test("linear eq system solve") {
    val system = Matrix.squareFromSeqs(IndexedSeq(
      IndexedSeq(4, 1, 0, 0, 0),
      IndexedSeq(1, 4, 1, 0, 0),
      IndexedSeq(0, 1, 4, 1, 0),
      IndexedSeq(0, 0, 1, 4, 1),
      IndexedSeq(0, 0, 0, 1, 4),
    ));

    val ds = IndexedSeq(1d, 2d, -4d, -2d, -3d)

    val solution = LinearSystemSolver.solveUnsafe(system, ds)
    println(solution)

    val solution2 = LinearSystemSolver.solveTridiagonal(
      IndexedSeq(1d, 1d, 1d, 1d, 1d),
      IndexedSeq(4d, 4d, 4d, 4d, 4d),
      IndexedSeq(1d, 1d, 1d, 1d, 1d),
      ds
    )
    println(solution2)

  assert(solution.zip(solution2).forall{case(f, s) => f ~= s})


  }

  test("segments clothest points and distance") {
    val st1 = V3(-1, 0, 0)
    val e1 = V3(1, 0, 0)
    val s1 = Segment(st1, e1)
    println(s1.clothestPoint(0d))
    assert(s1.clothestPoint(0d) ~= V3(0d))
    assert(s1.clothestPoint(e1) ~= e1)
    assert(s1.clothestPoint(st1) ~= st1)
    assert(s1.clothestPoint(V3(0, 10, 0)) ~= V3(0d))
    assert(s1.clothestPoint(V3(0, 0, 10)) ~= V3(0d))

    assert(s1.distanceTo(V3(0, 0, 0)) ~= (0d))
    assert(s1.distanceTo(V3(0, 10, 0)) ~= (10d))
    assert(s1.distanceTo(V3(-10, 0, 0)) ~= (9d))
    assert(s1.distanceTo(V3(10, 0, 0)) ~= (9d))
  }

  test("transform no rotate") {
    assert(Transform(0d).transformVector(0d) ~= V3(0d))
    assert(Transform(0d).transformPosition(0d) ~= V3(0d))
    assert(Transform(0d).transformVector(5d) ~= V3(5d))
    assert(Transform(0d).transformPosition(5d) ~= V3(5d))
    assert(Transform(0d).transformVector(-5d) ~= -V3(5d))
    assert(Transform(0d).transformPosition(-5d) ~= -V3(5d))

    assert(Transform(5d).transformVector(0d) ~= V3(0d))
    assert(Transform(5d).transformPosition(0d) ~= V3(5d))
    assert(Transform(5d).transformVector(5d) ~= V3(5d))
    assert(Transform(5d).transformPosition(5d) ~= V3(10d))
  }

  test("quaternion tests") {
    println(Rotations.aroundXPI.squared)
    println(Rotations.aroundYPI.squared)
    println(Rotations.aroundZPI.squared)
  }

  test("quaternion powers vs slerps") {
    val q = Rotations.aroundXPI.powUnit(0.5f)
    //val q = Quat(1f, 1f,0f,0f).normalized
    assert(q.squared ~= q.powUnit(2f))
    println("original " + q)
    println("---------------")
    println(Quat.slerpNomalized(Rotations.idRotation, q, 0.0f))
    println(Quat.slerpNomalized(Rotations.idRotation, q, 0.1f))
    println(Quat.slerpNomalized(Rotations.idRotation, q, 0.3f))
    println(Quat.slerpNomalized(Rotations.idRotation, q, 0.5f))
    println(Quat.slerpNomalized(Rotations.idRotation, q, 0.7f))
    println(Quat.slerpNomalized(Rotations.idRotation, q, 0.9f))
    println(Quat.slerpNomalized(Rotations.idRotation, q, 1f))
    println("------------------")
    println(q.powUnit(0.0f))
    println(q.powUnit(0.1f))
    println(q.powUnit(0.3f))
    println(q.powUnit(0.5f))
    println(q.powUnit(0.7f))
    println(q.powUnit(0.9f))
    println(q.powUnit(1f))
    println("---------------")
    println(Quat.fastLerpNormalized(Rotations.idRotation, q, 0.0f))
    println(Quat.fastLerpNormalized(Rotations.idRotation, q, 0.1f))
    println(Quat.fastLerpNormalized(Rotations.idRotation, q, 0.3f))
    println(Quat.fastLerpNormalized(Rotations.idRotation, q, 0.5f))
    println(Quat.fastLerpNormalized(Rotations.idRotation, q, 0.7f))
    println(Quat.fastLerpNormalized(Rotations.idRotation, q, 0.9f))
    println(Quat.fastLerpNormalized(Rotations.idRotation, q, 1f))
    assert(Quat.slerpNomalized(Rotations.idRotation, q, 0.3f) ~= q.powUnit(0.3f))
    //    assert(Quat.fastLerpNormalized(Rotation.idRotation, q, 0.3f) ~= q.powUnit(0.3f))
    //   assert(Quat.fastLerpNormalized(Rotation.idRotation, q, 0.3f) ~= Quat.slerpNomalized(Rotation.idRotation, q, 0.3f))

  }

}
