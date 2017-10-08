package traingame.editor

import traingame.Scenario
import traingame.TrainGameComponent
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import traingame.V2
import traingame.MutableScenario
import java.awt.event.MouseMotionListener
import java.awt.event.KeyEvent
import java.awt.event.InputEvent
import javax.swing.JOptionPane
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.BasicStroke
import java.awt.geom.Line2D
import java.awt.Color

class TrainEditorComponent[Scen <: MutableScenario](val scen: Scen) extends TrainGameComponent[Scen](scen) {
  var dragging: Option[(Scen#City, V2)] = None
  var joining: Option[Scen#City] = None
  var mpos = V2(0, 0)
  addMouseListener(new MouseListener() {
    def mouseClicked(ev: MouseEvent) = {
      val pt = V2.fromPoint(ev.getPoint).transform(screenspaceToBoardspace)
      if (selection == NoSelection && (ev.getModifiersEx & InputEvent.CTRL_DOWN_MASK) != 0) {
        scen.cities += new scen.City(pt, JOptionPane.showInputDialog("City Name"))
      }
      repaint()
    }
    def mouseEntered(ev: MouseEvent) = {}
    def mouseExited(ev: MouseEvent) = {}
    def mousePressed(ev: MouseEvent) = {
      val pt = V2.fromPoint(ev.getPoint).transform(screenspaceToBoardspace)
      if (identify(pt) == selection) {
        selection match {
          case CitySelection(city) =>
            if ((ev.getModifiersEx & InputEvent.SHIFT_DOWN_MASK) != 0) {
              joining = Some(city)
            }
            else {
              dragging = Some((city, city.pos - pt))
            }
          case _ =>
        }
      }
    }
    def mouseReleased(ev: MouseEvent) = {
      dragging = None
      (joining, identify(V2.fromPoint(ev.getPoint).transform(screenspaceToBoardspace))) match {
        case (Some(city1), CitySelection(city2)) if city1 != city2 =>
          scen.edges += new scen.Connection(city1.asInstanceOf[scen.City], city2.asInstanceOf[scen.City], None, 5)
        case _ =>
      }
      joining = None
      repaint()
    }
  })
  addMouseMotionListener(new MouseMotionListener() {
    def mouseMoved(ev: MouseEvent) = {}
    def mouseDragged(ev: MouseEvent) = {
      dragging match {
        case None =>
        case Some((city, diff)) =>
          val pt = V2.fromPoint(ev.getPoint).transform(screenspaceToBoardspace)
          val p = pt + diff
          city.pos = V2(p.x max 0 min scen.width, p.y max 0 min scen.height)
      }
      joining match {
        case None =>
        case Some(_) =>
          mpos = V2.fromPoint(ev.getPoint).transform(screenspaceToBoardspace)
      }
      repaint()
    }
  })
  setFocusable(true)
  
  override def paintComponent(graphics: Graphics) = {
    super.paintComponent(graphics)
    val gfx = graphics.asInstanceOf[Graphics2D]
    val tf = gfx.getTransform
    gfx.setTransform(boardspaceToScreenspace)
    joining match {
      case None =>
      case Some(city) =>
        gfx.setColor(Color.BLACK)
        val stroke = gfx.getStroke
        gfx.setStroke(new BasicStroke(0.1f))
        gfx.draw(new Line2D.Double(city.pos.x, city.pos.y, mpos.x, mpos.y))
        val d = (city.pos - mpos).unit
        val dl = d + d.cross/2
        val dr = d - d.cross/2
        gfx.draw(new Line2D.Double(mpos.x, mpos.y, (mpos + dl).x, (mpos + dl).y))
        gfx.draw(new Line2D.Double(mpos.x, mpos.y, (mpos + dr).x, (mpos + dr).y))
        gfx.setStroke(stroke)
    }
    gfx.setTransform(tf)
  }
}