package traingame.editor.util

import java.awt.Color
import javax.swing.Icon
import java.awt.Graphics
import java.awt.Component
import traingame.ConnColor

class ColorIcon(color: Color) extends Icon {
  
  def paintIcon(c: Component, g: Graphics, x: Int, y: Int) = {
    val dy = c.getHeight / 2 - 5
    g.setColor(Color.BLACK)
    g.drawRect(1, 0 + dy, 9, 9)
    g.setColor(color)
    g.fillRect(2, 1 + dy, 8, 8)
  }
  def getIconWidth = 11
  def getIconHeight = 10
  
}
object ColorIcon {
  private val map = ConnColor.connColors.map(c => c -> new ColorIcon(c.toAWT)).toMap
  private val greyIcon = new ColorIcon(Color.GRAY)
  def forColor(col: ConnColor) = map(col)
  def forOptColor(col: Option[ConnColor]) = col.map(forColor _).getOrElse(greyIcon)
}