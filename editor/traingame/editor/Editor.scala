package traingame.editor

import traingame._
import javax.swing.JFrame
import traingame.io.ScenarioIO
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.BorderFactory
import java.awt.GridLayout
import javax.swing.JSpinner
import javax.swing.JLabel
import javax.swing.SpinnerNumberModel
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import java.awt.Frame
import javax.swing.JMenuBar
import javax.swing.JMenu
import java.awt.Color
import javax.swing.WindowConstants
import java.awt.CardLayout
import javax.swing.BoxLayout
import javax.swing.Box
import javax.swing.JTextField
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.Panel
import javax.swing.JSeparator
import javax.swing.SwingConstants
import javax.swing.JComboBox
import javax.swing.ListCellRenderer
import javax.swing.DefaultListCellRenderer
import javax.swing.JList
import traingame.editor.util.ColorIcon
import traingame.io.parsers
import javax.swing.JButton
import javax.swing.JOptionPane
import javax.swing.JMenuItem
import javax.swing.KeyStroke
import javax.swing.JFileChooser
import java.io.File
import java.io.IOException
import java.io.FileWriter
import traingame.io.JSON

object Editor {
  
  def main(args: Array[String]) = {
    val w = 40.0
    val h = 40.0
    val scenario = new Scenario("My Scenario", w, h)
    val northville = new City(V2(w/2, 0), "Northville")
    scenario.cities += northville
    scenario.cities += new City(V2(w/2, h), "Southburg")
    scenario.cities += new City(V2(0, h/2), "Westtown")
    val eastdale = new City(V2(w, h/2), "Eastdale")
    scenario.cities += eastdale
    scenario.cities += new City(V2(w/2, h/2), "Middleton")
    scenario.edges += new Connection(northville, eastdale, Some(ConnColor.Green), 6)
    println(ScenarioIO.save(scenario))
    open(scenario)
  }
  def open(scenario: Scenario) = {
    val panel = new TrainEditor(scenario)
    val frame = new JFrame(scenario.name)
    frame.setJMenuBar(panel.menuBar)
    frame.add(panel)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.setSize(200, 200)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    ()
  }
  
}
class TrainEditor(val scenario: Scenario) extends JPanel { self =>
  
  setLayout(new BorderLayout())
  
  val comp = new TrainEditorComponent(scenario)
  add(comp, BorderLayout.CENTER)
  
  val sidebar = new JPanel()
  sidebar.setLayout(new BoxLayout(sidebar, BoxLayout.Y_AXIS))
  val fields = new JPanel()
  add({
    val cont = new JPanel()
    cont.setLayout(new BorderLayout())
    cont.add(new JPanel(), BorderLayout.CENTER)
    cont.add(sidebar, BorderLayout.NORTH)
    cont.setBorder(BorderFactory.createEtchedBorder())
    cont
  }, BorderLayout.EAST)
  
  fields.setLayout(new GridLayout(0, 2, 5, 5))
  fields.add(new JLabel("Width: "))
  val widthBox = new JSpinner(new SpinnerNumberModel(scenario.width, 5, 160, 1))
  fields.add(widthBox)
  fields.add(new JLabel("Height: "))
  val heightBox = new JSpinner(new SpinnerNumberModel(scenario.height, 5, 85, 1))
  fields.add(heightBox)
  val sizeChangeListener = new ChangeListener() {
    def stateChanged(ev: ChangeEvent) = {
      scenario.width = widthBox.getValue.asInstanceOf[Double].toInt
      scenario.height = heightBox.getValue.asInstanceOf[Double].toInt
      comp.repaint()
    }
  }
  widthBox.addChangeListener(sizeChangeListener)
  heightBox.addChangeListener(sizeChangeListener)
  sidebar.add(fields)
  
  sidebar.add(Box.createVerticalStrut(5))
  sidebar.add(new JSeparator(SwingConstants.HORIZONTAL))
  sidebar.add(Box.createVerticalStrut(5))
  
  val cityEditPanel = new JPanel()
  val connEditPanel = new JPanel()
  val emptyEditLabel = new JLabel("<html>Select an entity<br>to edit it")
  val editPanel = new JPanel()
  val editCards = new CardLayout()
  editPanel.setLayout(editCards)
  editPanel.add(emptyEditLabel, "none")
  editPanel.add(cityEditPanel, "city")
  editPanel.add(connEditPanel, "conn")
  sidebar.add(editPanel)
  sidebar.add(Box.createGlue())
  comp.addSelectionChangedListener((_, sel) => {
    sel match {
      case TrainGameComponent.NoSelection => editCards.show(editPanel, "none")
      case TrainGameComponent.CitySelection(city) =>
        editCards.show(editPanel, "city")
        setCityData(city)
      case TrainGameComponent.ConnSelection(conn) =>
        editCards.show(editPanel, "conn")
        setConnData(conn)
    }
  })
  
  cityEditPanel.setLayout(new BorderLayout())
  cityEditPanel.add(new JPanel(), BorderLayout.CENTER)
  val cityEditSubpanel = new JPanel()
  cityEditPanel.add(cityEditSubpanel, BorderLayout.NORTH)
  cityEditSubpanel.setLayout(new GridLayout(0, 2, 5, 5))
  cityEditSubpanel.add(new JLabel("Name: "))
  val cityNameBox = new JTextField()
  cityNameBox.setColumns(8)
  cityEditSubpanel.add(cityNameBox)
  cityNameBox.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) = {
      val TrainGameComponent.CitySelection(city) = comp.selection
      city.name = cityNameBox.getText
      comp.repaint()
    }
  })
  val removeCity = new JButton("Remove")
  cityEditSubpanel.add(removeCity)
  removeCity.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) = {
      val TrainGameComponent.CitySelection(city) = comp.selection
      for (conn <- scenario.connections(city)) {
        scenario.edges -= conn
      }
      scenario.cities -= city
      comp.selection = TrainGameComponent.NoSelection
      comp.repaint()
    }
  })
  def setCityData(city: City) = {
    cityNameBox.setText(city.name)
  }
  
  connEditPanel.setLayout(new BorderLayout())
  connEditPanel.add(new JPanel(), BorderLayout.CENTER)
  val connEditSubpanel = new JPanel()
  connEditPanel.add(connEditSubpanel, BorderLayout.NORTH)
  connEditSubpanel.setLayout(new GridLayout(0, 2, 5, 5))
  connEditSubpanel.add(new JLabel("Length: "))
  val connLenBox = new JSpinner(new SpinnerNumberModel(4, 1, 6, 1))
  connEditSubpanel.add(connLenBox)
  connLenBox.addChangeListener(new ChangeListener() {
    def stateChanged(ev: ChangeEvent) = {
      val TrainGameComponent.ConnSelection(conn) = comp.selection
      conn.length = connLenBox.getValue.asInstanceOf[Integer]
      comp.repaint()
    }
  })
  connEditSubpanel.add(new JLabel("Color: "))
  val connColorBox = new JComboBox((None +: ConnColor.connColors.map(Some(_))).toArray)
  connColorBox.setRenderer(new ListCellRenderer[Option[ConnColor]]() {
    val renderer = new DefaultListCellRenderer()
    def getListCellRendererComponent(list: JList[_ <: Option[ConnColor]], value: Option[ConnColor], index: Int, isSelected: Boolean, cellHasFocus: Boolean) = {
      renderer.asInstanceOf[ListCellRenderer[Object]].getListCellRendererComponent(list, value, index, isSelected, cellHasFocus)
      renderer.setText(value.map(_.toString).getOrElse("Grey"))
      renderer.setIcon(ColorIcon.forOptColor(value))
      renderer
    }
  })
  connColorBox.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) {
      val TrainGameComponent.ConnSelection(conn) = comp.selection
      conn.color = connColorBox.getSelectedItem.asInstanceOf[Option[ConnColor]]
      comp.repaint()
    }
  })
  connEditSubpanel.add(connColorBox)
  val removeConn = new JButton("Remove")
  connEditSubpanel.add(removeConn)
  removeConn.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) = {
      val TrainGameComponent.ConnSelection(conn) = comp.selection
      scenario.edges -= conn
      comp.selection = TrainGameComponent.NoSelection
      comp.repaint()
    }
  })
  def setConnData(conn: Connection) = {
    connLenBox.setValue(conn.length)
    connColorBox.setSelectedItem(conn.color)
  }
  
  sidebar.add(Box.createVerticalStrut(5))
  sidebar.add(new JSeparator(SwingConstants.HORIZONTAL))
  
  val menuBar = new JMenuBar()
  val fileMenu = new JMenu("File")
  menuBar.add(fileMenu)
  val saveButton = new JMenuItem("Save")
  fileMenu.add(saveButton)
  saveButton.setAccelerator(KeyStroke.getKeyStroke("control S"))
  saveButton.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) = {
      val scendir = new File("./TrainGame/scenarios")
      scendir.mkdirs()
      val fc = new JFileChooser(scendir)
      if (fc.showSaveDialog(self) == JFileChooser.APPROVE_OPTION) {
        val file = fc.getSelectedFile
        var stream = null: FileWriter
        try {
          stream = new FileWriter(file)
          stream.write(ScenarioIO.save(scenario).toString())
        }
        catch {
          case ex: IOException =>
            JOptionPane.showMessageDialog(self, "Error during saving: " + ex.getMessage, "Save Error", JOptionPane.ERROR_MESSAGE)
        }
        finally if (stream != null) {
          stream.close()
        }
      }
    }
  })
  val openButton = new JMenuItem("Open")
  fileMenu.add(openButton)
  openButton.setAccelerator(KeyStroke.getKeyStroke("control O"))
  openButton.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) = {
      val scendir = new File("./TrainGame/scenarios")
      scendir.mkdirs()
      val fc = new JFileChooser(scendir)
      if (fc.showOpenDialog(self) == JFileChooser.APPROVE_OPTION) {
        val file = fc.getSelectedFile
        try {
          Editor.open(ScenarioIO.load(parsers.parse(JSON.parser.json, file)))
        }
        catch {
          case ex: IOException =>
            JOptionPane.showMessageDialog(self, "Error during opening: " + ex.getMessage, "Open Error", JOptionPane.ERROR_MESSAGE)
            ex.printStackTrace()
          case ex: Exception =>
            JOptionPane.showMessageDialog(self, "Error during parsing: " + ex.getMessage, "Open Error", JOptionPane.ERROR_MESSAGE)
            ex.printStackTrace()
        }
      }
    }
  })
  
}