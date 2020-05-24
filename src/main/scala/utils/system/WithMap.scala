package utils.system

trait WithMap {
  def map[B](f:this.type => B):B = f(this)
}
