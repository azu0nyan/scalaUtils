package utils.math.planar.algo.straightSkeleton

import org.scalatest.matchers.should
import org.scalatest.flatspec.AnyFlatSpec
import utils.math.planar.algo.straightSkeleton.math.LinearForm3D
import utils.math.space.V3

class TestLinearForm extends AnyFlatSpec with should.Matchers {

  "LinearForm3D" should "work" in {
    //0.7071067811865476,-0.0,0.7071067811865475,-353.5533905932738 -0.5000000000000001,-0.5000000000000001,0.7071067811865476,150.00000000000006 0.5000000000000001,-0.5000000000000001,0.7071067811865476,-350.0000000000001


//      0.7071067811865476,-0.0,0.7071067811865475,-353.5533905932738
//    b: -0.5,-0.5,0.7071067811865474,150.0
//    c: 0.5,-0.5,0.7071067811865474,-350.0
    val lForm1 = new LinearForm3D(0.7071067811865476, -0.0, 0.7071067811865475, -353.5533905932738)

    val lForm2 = new LinearForm3D(-0.5000000000000001, -0.5000000000000001, 0.7071067811865476, 150.00000000000006)

    val lForm3 = new LinearForm3D(0.5000000000000001, -0.5000000000000001, 0.7071067811865476, -350.0000000000001)


    val expected = V3(500.00000000000006, -200.00000000000009, -5.684341886080799E-14)
    val res = lForm1.collide(lForm2, lForm3)


    println(res)
    assert(res.exists(_ == expected))


  }
}
