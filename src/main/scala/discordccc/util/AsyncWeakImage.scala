package discordccc.util

import ccc.JavafxExecutionContext
import ccc.util.WeakImage
import headache.AhcUtils
import java.io.ByteArrayInputStream
import javafx.scene.image.Image
import org.asynchttpclient.AsyncHttpClient

class AsyncWeakImageFactory(ahc: AsyncHttpClient) {

  def asyncWeakImage(imageLocation: String,
                     requestedWidth: Double = 0, requestedHeight: Double = 0,
                     preserveRatio: Boolean = true, smooth: Boolean = true,
                     backgroundLoading: Boolean = true) = new WeakImage(imageLocation, requestedWidth, requestedHeight, preserveRatio, smooth, backgroundLoading) {
    override def fetchImage() = {
      if (imageLocation.startsWith("http")) AhcUtils.request(ahc.prepareGet(imageLocation))(_.getResponseBodyAsBytes).map(bytes => 
        new Image(new ByteArrayInputStream(bytes)))(JavafxExecutionContext)
      else super.fetchImage()
    }
  }
}
