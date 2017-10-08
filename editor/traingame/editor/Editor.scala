package traingame.editor

import traingame.MutableScenario
import traingame.TrainGameComponent
import javax.swing.JFrame
import traingame.io.ScenarioIO
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.BorderFactory
import java.awt.GridLayout
import javax.swing.JSpinner
import javax.swing.JLabel
import traingame.ConnColor
import traingame.V2
import javax.swing.SpinnerNumberModel
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import java.awt.Frame
import javax.swing.JMenuBar
import javax.swing.JMenu
import java.awt.Color

object Editor {
  
  def main(args: Array[String]) = {
    val w = 40.0
    val h = 40.0
    val scenario = new MutableScenario("My Scenario", w, h)
    val northville = new scenario.City(V2(w/2, 0), "Northville")
    scenario.cities += northville
    scenario.cities += new scenario.City(V2(w/2, h), "Southburg")
    scenario.cities += new scenario.City(V2(0, h/2), "Westtown")
    val eastdale = new scenario.City(V2(w, h/2), "Eastdale")
    scenario.cities += eastdale
    scenario.cities += new scenario.City(V2(w/2, h/2), "Middleton")
    scenario.edges += new scenario.Connection(northville, eastdale, Some(ConnColor.Green), 7)
    println(ScenarioIO.save(scenario))
    val panel = new TrainEditor(scenario)
    val frame = new JFrame(scenario.name)
    frame.setJMenuBar(panel.menuBar)
    frame.add(panel)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(200, 200)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    ()
  }
  
}
class TrainEditor(val scenario: MutableScenario) extends JPanel {
  
  setLayout(new BorderLayout())
  
  val comp = new TrainEditorComponent[scenario.type](scenario)
  add(comp, BorderLayout.CENTER)
  
  val sidebar = new JPanel()
  add({
    val cont = new JPanel()
    cont.setLayout(new BorderLayout())
    cont.add(new JPanel(), BorderLayout.CENTER)
    cont.add(sidebar, BorderLayout.NORTH)
    cont.setBorder(BorderFactory.createEtchedBorder())
    cont
  }, BorderLayout.EAST)
  
  sidebar.setLayout(new GridLayout(0, 2, 5, 5))
  sidebar.add(new JLabel("Width: "))
  val widthBox = new JSpinner(new SpinnerNumberModel(scenario.width, 5, 160, 1))
  sidebar.add(widthBox)
  sidebar.add(new JLabel("Height: "))
  val heightBox = new JSpinner(new SpinnerNumberModel(scenario.height, 5, 85, 1))
  sidebar.add(heightBox)
  val sizeChangeListener = new ChangeListener() {
    def stateChanged(ev: ChangeEvent) = {
      scenario.width = widthBox.getValue.asInstanceOf[Double].toInt
      scenario.height = heightBox.getValue.asInstanceOf[Double].toInt
      comp.repaint()
    }
  }
  widthBox.addChangeListener(sizeChangeListener)
  heightBox.addChangeListener(sizeChangeListener)
  
  val menuBar = new JMenuBar()
  val fileMenu = new JMenu("File")
  menuBar.add(fileMenu)
  
}