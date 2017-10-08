package traingame

import java.awt.geom.Point2D
import java.awt.Point
import java.awt.geom.AffineTransform

case class V2(x: Double, y: Double) {
  
  def +(that: V2) = V2(this.x + that.x, this.y + that.y)
  def unary_- = V2(-x, -y)
  def -(that: V2) = this + (-that)
  def *(that: V2) = this.x * that.x + this.y * that.y
  def cross = V2(-y, x)
  def cp(that: V2) = this * that.cross
  def *(that: Double) = V2(x * that, y * that)
  def /(that: Double) = V2(x / that, y / that)
  def norm2 = this * this
  def norm = math.sqrt(norm2)
  def unit =
    if (norm2 < 1e-12) V2(0, 0) else this / norm
  def toPoint = new Point2D.Double(x, y)
  def transform(tf: AffineTransform) = V2.fromPoint2D(tf.transform(toPoint, new Point2D.Double))
  
}

object V2 {
  
  def fromPoint(pt: Point) = V2(pt.getX, pt.getY) 
  def fromPoint2D(pt: Point2D) = V2(pt.getX, pt.getY) 
  
}