import javafx.scene.text.Font
import scala.concurrent.Future
import javafx.scene.control.Alert

package object discordccc {

  import ccc.JavafxExecutionContext.context
  implicit class ShowAlertDialogOnFailure[T](private val f: Future[T]) extends AnyVal {
    def showAlertOnFailure(msg: Throwable => String): Unit = f.failed.foreach { ex => 
      new Alert(Alert.AlertType.ERROR, msg(ex)).showAndWait()
    }
  }

  implicit class EmUnits(private val unit: Double) extends AnyVal {
    @inline def em = Font.getDefault.getSize * unit
  }
}
