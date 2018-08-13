package discordccc

import ccc._
import javafx.beans.value.ChangeListener
import javafx.event.ActionEvent
import javafx.scene.control.{ButtonType, ButtonBar, Dialog, TextField, Button, ProgressBar, Label}
import javafx.scene.image.ImageView
import javafx.scene.paint.Color
import org.asynchttpclient.AsyncHttpClient
import JavafxExecutionContext.context

class DiscordLoginDialog(ahc: AsyncHttpClient, token: Option[String] = None) extends Dialog[connector.DiscordConnector] {
  private val loginButtonType = new ButtonType("Login", ButtonBar.ButtonData.OK_DONE)
  getDialogPane.getButtonTypes.addAll(loginButtonType, ButtonType.CANCEL)
  
  setGraphic(new ImageView("/discordmaterial.png").modify(_.setSmooth(true), _.setFitWidth(200), _.setPreserveRatio(true)))
  setTitle("Insert discord user token")
  
  private val tokenTextField = new TextField().modify(_ setPromptText "token", _.setMaxWidth(Double.MaxValue), _ setPrefColumnCount 30, _.setText(token.getOrElse("")))
  private val loadingIndicator = new ProgressBar().modify(_ setVisible false, _ setMaxWidth Double.MaxValue)
  private val statusMessage = new Label().modify(_ setTextFill Color.RED, _ setVisible false)
  getDialogPane setContent vbox(tokenTextField, statusMessage, loadingIndicator)(spacing = 20, fillWidth = true)
  
  val loginButton = getDialogPane.lookupButton(loginButtonType).asInstanceOf[Button]
  tokenTextField.textProperty.isEmpty foreach (b => loginButton.setDisable(b))
  loginButton.addEventFilter[ActionEvent](ActionEvent.ACTION, evt => {
      evt.consume()
      if (!tokenTextField.getText.isEmpty) {
        loginButton setDisable true
        loadingIndicator setVisible true
        
        val client = new connector.DiscordConnector(tokenTextField.getText, ahc)
        client.init() onComplete { result =>
          loadingIndicator setVisible false
          result.fold(ex => {
              ex.printStackTrace
              statusMessage setText ex.getMessage
              statusMessage setVisible true
              lazy val textChangeListener: ChangeListener[String] = (_, _, newText) => {
                tokenTextField.textProperty.removeListener(textChangeListener)
                statusMessage setVisible false
                loginButton setDisable false
              }
              tokenTextField.textProperty.addListener(textChangeListener)
            }, conns => {
              println("got connections " + conns)
              setResult(client)
              close()
            })
        }
      }
    })
  
  setResultConverter(button => null) //this can only be triggered by the cancel button

  if (token.nonEmpty) {
    loginButton.fire()
  }
}
