package discordccc

import ccc._
import discordccc.model._
import javafx.scene.control.TextField
import javafx.scene.input.MouseButton
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.stage.Popup

object MemberRender extends (Member => Node) {

  def apply(m: Member): Node = {
    m.connector.getUser(m.userId) match {
      case Some(user) =>
        val nameLabel = new Label(user.name).modify(_.setStyle(f"-fx-text-fill: #${m.color}%06X"), _.getStyleClass add "chat-list-member-name")
        nameLabel.setOnMouseClicked { evt =>
          if (evt.getButton == MouseButton.PRIMARY) {
            val popup = new Popup()
            
            def nonEditableTextField(text: String) = {
              val res = new TextField(text)
              res.setEditable(false)
              res
            }
            popup.getContent.add(vbox(
                nonEditableTextField(if (user.bot) "BOT" else user.name + "#" + user.extra),
                nonEditableTextField("ID: " + java.lang.Long.toUnsignedString(user.id))
              ))
            
            popup.setHideOnEscape(true)
            popup.setAutoHide(true)
            val location = nameLabel.localToScreen(0, 0)
            popup.show(nameLabel, location.getX, location.getY)
          }
        }
        

        if (user.name.trim != m.nickname.trim) hbox(nameLabel, new Label("(" + m.nickname + ")").modify(_.getStyleClass add "chat-list-member-nickname"))(spacing = 5)
        else nameLabel
      case _ => new Label(m.nickname).modify(_.setStyle(f"-fx-text-fill: #${m.color}%06X"), _.getStyleClass add "chat-list-member-name")
    }
  }
}
