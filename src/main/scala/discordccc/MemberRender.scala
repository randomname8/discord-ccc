package discordccc

import ccc._
import discordccc.model._
import javafx.scene.Node
import javafx.scene.control.Label

object MemberRender extends (Member => Node) {

  def apply(m: Member): Node = {
    m.connector.getUser(m.userId).map(_.name) match {
      case Some(name) => 
        val nameLabel = new Label(name).modify(_.setStyle(f"-fx-text-fill: #${m.color}%06X"), _.getStyleClass add "chat-list-member-name")
        if (name.trim != m.nickname.trim) hbox(nameLabel, new Label("(" + m.nickname + ")").modify(_.getStyleClass add "chat-list-member-nickname"))(spacing = 5)
        else nameLabel
      case _ => new Label(m.nickname).modify(_.setStyle(f"-fx-text-fill: #${m.color}%06X"), _.getStyleClass add "chat-list-member-name")
    }
  }
}
