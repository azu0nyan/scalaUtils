package utils.math.planar.algo.straightSkeleton


import java.awt.Color
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.Comparator
import javax.swing.ButtonGroup
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu
import javax.swing.JRadioButtonMenuItem

/**
 * "Tags" for output properties
 *
 * @author twak
 */
object Feature {
   val rainbow: Rainbow = null
  var nameComparator = new Comparator[Feature]() {
    override def compare(o1: Feature, o2: Feature) = String.CASE_INSENSITIVE_ORDER.compare(o1.name, o2.name)
  }
}
class Feature(var name: String) {
  color = Rainbow.next(this.getClass)
  colorName = Rainbow.lastAsString(this.getClass)
  var color: Color = null
   var colorName: String = null
  override def toString = name + "(" + colorName + ")"
  /**
   * This menu is shown when someone clicks on the marker
   *
   * @return
   */
  def createMenu(m: Marker) = {
    val popup = new JPopupMenu
    val menu = new JMenu("type:")
    popup.add(menu)
    val group = new ButtonGroup
    for (t <- Marker.Type.values) {
      val tFinal = t
      val item = new JRadioButtonMenuItem(t.toString)
      if (m.properties.get(Marker.TYPE) eq t) item.setSelected(true)
      item.addActionListener(new ActionListener() {
        override def actionPerformed(arg0: ActionEvent): Unit = {
          if (item.isSelected) m.properties.put(Marker.TYPE, tFinal)
        }
      })
      menu.add(item)
      group.add(item)
    }
    popup
  }
}

