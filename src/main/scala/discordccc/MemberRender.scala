package discordccc

import ccc.Modifier
import javafx.scene.Node
import javafx.scene.control.Label

object MemberRender extends (Member => Node) {

  def apply(m: Member): Node = new Label(m.nickname).modify(_.setStyle(f"-fx-text-fill: #${m.color}%06X"))
}
