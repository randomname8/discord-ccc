package discordccc

import discordccc.model.Message
import javafx.scene.Node
import javafx.scene.control.Button
import javafx.scene.control.TextField
import javafx.stage.Popup

object MessageExtraControls extends (Message => Seq[Node]) {
  def apply(msg: Message) = {
    val infoBtn = new Button("ℹ")
    infoBtn.setOnAction { _ =>
      val popup = new Popup()
            
      def nonEditableTextField(text: String) = {
        val res = new TextField(text)
        res.setEditable(false)
        res
      }
      popup.getContent.add(nonEditableTextField("Msg ID: " + java.lang.Long.toUnsignedString(msg.id)))
            
      popup.setHideOnEscape(true)
      popup.setAutoHide(true)
      val location = infoBtn.localToScreen(0, 0)
      popup.show(infoBtn, location.getX, location.getY)
    }
    Seq(infoBtn)
  }
}
