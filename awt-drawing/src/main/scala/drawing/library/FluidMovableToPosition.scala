package drawing.library

import drawing.core.DrawableUpdatable
import utils.math._
import utils.math.planar.V2

trait FluidMovableToPosition extends DrawableUpdatable {

  var targetPos: V2 = V2.ZERO
  var speed: Double = 1f
  var pos: V2 = V2.ZERO
  var firstUpdate:Boolean = true


  /** вызывается до рисования каждый кадр */
  override def update(dt: Scalar): Unit = {
    if(firstUpdate){
      firstUpdate = false
      pos = targetPos
    } else {
      val currentToTarget: V2 = targetPos - pos
      val distDelta: Scalar = min(currentToTarget.length, dt * speed)
      pos += currentToTarget * distDelta
    }
  }

}
