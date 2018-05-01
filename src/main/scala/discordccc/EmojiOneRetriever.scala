package discordccc

import better.files._
import ccc._
import javafx.application.Platform
import javafx.geometry.Insets
import javafx.scene.Scene
import javafx.scene.control._
import javafx.scene.layout._
import javafx.stage.{Stage, StageStyle}
import headache.AhcUtils
import org.asynchttpclient.AsyncHttpClient

object EmojiOneRetriever {
  
  def downloadEmojiOne(ahc: AsyncHttpClient, emojioneDir: File): Unit = {
    val loadingDialog = new Stage(StageStyle.UNDECORATED)
    val progressBar = new ProgressBar().modify(BorderPane.setMargin(_, new Insets(5)), _ setMaxWidth Double.MaxValue)
    val statusLabel = new Label("setting up...").modify(BorderPane.setMargin(_, new Insets(5)))
    loadingDialog setScene new Scene(new BorderPane().modify(_ setTop progressBar, _ setBottom statusLabel))
      
    AhcUtils.request(ahc.prepareGet("https://d1j8pt39hxlh3d.cloudfront.net/emoji/emojione/3.1.1/EmojiOne_3.1.1_128x128_png.zip")) { response =>
      if (response.getStatusCode == 200) {
        val zipFile = emojioneDir/"emojione.zip"
        zipFile.outputStream(File.OpenOptions.append) foreach { zipFile =>
          val byteStream = response.getResponseBodyAsStream
          val totalBytes = response.getHeader("Content-Length").toLong
          val buffer = new Array[Byte](1024*4)
          var readSoFar = 0
          var in = 0
          while ({in = byteStream.read(buffer); in >= 0}) {
            zipFile.write(buffer, 0, in)
            readSoFar += in
            Platform.runLater { () => progressBar setProgress readSoFar / totalBytes.toDouble}
          }
        }
        zipFile.unzipTo(emojioneDir)
        zipFile.delete(true)
        Platform.runLater { () => loadingDialog.close() }
      } else {
        Platform.runLater { () =>
          loadingDialog.close()
          new Alert(Alert.AlertType.ERROR, s"Failed downloading emojis: ${response.getStatusCode} - ${response.getStatusText}").showAndWait()
          sys.exit(1)
        }
      }
    }
    
    loadingDialog setWidth 400
    loadingDialog setHeight 300
    loadingDialog.centerOnScreen()
    loadingDialog.showAndWait()
  }
}
