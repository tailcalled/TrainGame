package traingame

import javax.swing.JComponent
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.geom.Ellipse2D
import java.awt.Color
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.awt.geom.Point2D
import java.awt.RenderingHints
import java.awt.geom.Rectangle2D
import java.awt.Font
import java.awt.event.ComponentListener
import java.awt.event.ComponentEvent
import java.awt.geom.Line2D

object TrainGameComponent {
  sealed trait Selection
  case object NoSelection extends Selection
  case class CitySelection(selected: City) extends Selection
  case class ConnSelection(selected: Connection) extends Selection
}
class TrainGameComponent(scen: Scenario) extends JComponent {
  import TrainGameComponent._
  private var _selection: Selection = NoSelection
  private var onSelectionChanged: (Selection, Selection) => Unit = (oldSel, newSel) => {}
  def selection = _selection
  def selection_=(sel: Selection) = {
    val oldSel = _selection
    _selection = sel
    onSelectionChanged(oldSel, sel)
  }
  def addSelectionChangedListener(listener: (Selection, Selection) => Unit) {
    val selChange = onSelectionChanged
    onSelectionChanged = (oldSel, newSel) => {
      selChange(oldSel, newSel)
      listener(oldSel, newSel)
    }
  }
  
  val cityDiam = 2.0
  val cityRad = cityDiam/2
  val trainWidth = cityDiam * 0.70
  val trainAR = 3
  val trainLength = trainAR * trainWidth
  
  def boardspaceToScreenspace = {
    var boardRatio = (scen.width + cityDiam) / (scen.height + cityDiam)
    var screenRatio = getWidth.toDouble / getHeight
    val tf = new AffineTransform
    val scale =
      if (boardRatio > screenRatio) getWidth / (scen.width + cityDiam)
      else /* boardRatio < screenRatio */ getHeight / (scen.height + cityDiam)
    tf.translate(getWidth.toDouble / 2, getHeight.toDouble / 2)
    tf.scale(scale, scale)
    tf.translate(-scen.width / 2, -scen.height / 2)
    tf
  }
  def screenspaceToBoardspace = {
    var boardRatio = (scen.width + cityDiam) / (scen.height + cityDiam)
    var screenRatio = getWidth.toDouble / getHeight
    val tf = new AffineTransform
    val scale =
      if (boardRatio > screenRatio) getWidth / (scen.width + cityDiam)
      else /* boardRatio < screenRatio */ getHeight / (scen.height + cityDiam)
    tf.translate(scen.width / 2, scen.height / 2)
    tf.scale(1/scale, 1/scale)
    tf.translate(-getWidth.toDouble / 2, -getHeight.toDouble / 2)
    tf
  }
  
  def identify(pt: V2): Selection = {
      for (city <- scen.cities) {
        val dp = pt - city.pos
        if (dp * dp < cityRad * cityRad) {
          return CitySelection(city)
        }
      }
      for (conn <- scen.edges) {
        val p0 = conn.from.pos
        val p1 = conn.to.pos
        val dp = p1 - p0
        val d = dp.unit * (pt - p0)
        if (d > 0 && d < dp.norm) {
          val dMark = dp.unit.cross * (pt - p0)
          if (dMark > -1 && dMark < 1) {
            return ConnSelection(conn)
          }
        }
      }
      NoSelection
  }
  
  addMouseListener(new MouseListener() {
    def mouseClicked(ev: MouseEvent) = {
      val pt = V2.fromPoint(ev.getPoint).transform(screenspaceToBoardspace)
      val oldSel = selection
      selection = identify(pt)
      repaint()
    }
    def mouseEntered(ev: MouseEvent) = {}
    def mouseExited(ev: MouseEvent) = {}
    def mousePressed(ev: MouseEvent) = {}
    def mouseReleased(ev: MouseEvent) = {}
  })
  
  addComponentListener(new ComponentListener() {
    def componentHidden(ev: ComponentEvent) = {}
    def componentMoved(ev: ComponentEvent) = {
      repaint()
    }
    def componentResized(ev: ComponentEvent) = {
      repaint()
    }
    def componentShown(ev: ComponentEvent) = {}
  })
  
  override def paintComponent(graphics: Graphics) = {
    val gfx = graphics.asInstanceOf[Graphics2D]
    gfx.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val tf = gfx.getTransform
    gfx.setTransform(boardspaceToScreenspace)
    gfx.setColor(Color.BLACK)
    gfx.fill(new Rectangle2D.Double(0, 0, scen.width, scen.height))
    gfx.setColor(Color.LIGHT_GRAY)
    gfx.fill(new Rectangle2D.Double(0.05, 0.05, scen.width - 0.1, scen.height - 0.1))
    for (conn <- scen.edges) {
      val p0 = conn.from.pos
      val p1 = conn.to.pos
      val dp = p1 - p0
      val targetLen = trainLength * conn.length + cityDiam
      val directLen = dp.norm
      val off = {
        val off_ = (directLen*directLen - targetLen * targetLen) / 4
        if (off_ < 0) math.sqrt(-off_) else 0 
      }
      def parabolic(d: Double) = {
        val adj = d + cityRad
        val mid = (adj / targetLen - 0.5) * 2
        val dmiddd = 2/targetLen
        val quad = mid*mid
        val dquaddd = 2 * mid * dmiddd
        val direct = (dp * mid) / 2 + dp/2
        val ddirectdd = dp * dmiddd / 2
        val orth = dp.cross.unit * off * (1 - quad)
        val dorthdd = dp.cross.unit * off * -dquaddd
        (direct + orth + p0, ddirectdd + dorthdd)
      }
      val tf0 = gfx.getTransform
      for (part <- 0 until conn.length) {
        val (t, dir) = parabolic((part + 0.5) * trainLength)
        gfx.translate(t.x, t.y)
        gfx.rotate(Math.atan2(dir.y, dir.x))
        val e = trainWidth * 0.04
        if (selection == ConnSelection(conn)) {
          gfx.setColor(Color.WHITE)
        gfx.fill(new Rectangle2D.Double(-trainLength/2 - e, -trainWidth/2 - e, trainLength + 2*e, trainWidth + 2*e))
        }
        gfx.setColor(Color.BLACK)
        gfx.fill(new Rectangle2D.Double(-trainLength/2, -trainWidth/2, trainLength, trainWidth))
        gfx.setColor(conn.color.map(_.toAWT).getOrElse(Color.GRAY))
        gfx.fill(new Rectangle2D.Double(-trainLength/2 + e, -trainWidth/2 + e, trainLength - 2*e, trainWidth - 2*e))
        gfx.setTransform(tf0)
      }
    }
    val cityFont = Font.decode("Arial SanSerif 1")
    gfx.setFont(cityFont)
    for (city <- scen.cities) {
      gfx.setColor(Color.BLACK)
      gfx.fill(new Ellipse2D.Double(city.pos.x - cityDiam/2, city.pos.y - cityDiam/2, cityDiam, cityDiam))
      gfx.setColor(Color.YELLOW)
      val d2 = cityDiam * 0.95
      gfx.fill(new Ellipse2D.Double(city.pos.x - d2/2, city.pos.y - d2/2, d2, d2))
      gfx.setColor(Color.BLACK)
      val d3 = cityDiam * 0.80
      gfx.fill(new Ellipse2D.Double(city.pos.x - d3/2, city.pos.y - d3/2, d3, d3))
      if (selection == CitySelection(city)) {
        gfx.setColor(Color.PINK)
      }
      else {
        gfx.setColor(Color.RED)
      }
      val d4 = cityDiam * 0.75
      gfx.fill(new Ellipse2D.Double(city.pos.x - d4/2, city.pos.y - d4/2, d4, d4))
      gfx.setColor(Color.BLACK)
      val fm = gfx.getFontMetrics
      val bounds = fm.getStringBounds(city.name, gfx)
      gfx.drawString(city.name, (city.pos.x - bounds.getWidth/2).toFloat, (city.pos.y - cityRad - bounds.getMaxY).toFloat)
    }
    gfx.setTransform(tf)
  }
  
}