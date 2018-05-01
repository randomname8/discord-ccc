package discordccc

import javafx.application.Platform
import scala.concurrent.ExecutionContext

object JavafxExecutionContext extends ExecutionContext {
  implicit val context = this
  override def execute(runnable: Runnable) = Platform.runLater(runnable)
  override def reportFailure(cause: Throwable) = ExecutionContext.defaultReporter(cause)
}
