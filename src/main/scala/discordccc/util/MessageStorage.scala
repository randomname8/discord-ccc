package discordccc
package util

import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.KryoBase
import java.io.ByteArrayOutputStream
import java.util.zip.GZIPOutputStream

/**
 * Compresses messages in sequential fashion.
 * 
 * This storage allows storage and iteration of the messages in the same order they were stored but keeps them internally compressed.
 * Given that messages are mostly text, this allows for a big save in RAM.
 */
final class MessageStorage(val kryo: KryoBase, val messagesPerPage: Int) {
  /**
   * Represents the current uncompleted page. Should always be the oldest messages.
   */
  private[this] val uncompletedPage = new collection.mutable.ArrayBuffer[Message](messagesPerPage)
  private[this] val pages = new collection.mutable.ArrayBuffer[Array[Byte]](10)
  
  /** Retrieves page `latest - i`, i = 0 means the most recent messages.
   *  A page has at most `messagePerPage` entries.
   */
  def page(i: Int): Seq[Message] = {
    if (pages.size == i) uncompletedPage.to[Vector]
    else if (pages.size > i) kryo.readObject(new Input(pages(i)), classOf[Array[Message]])
    else Seq.empty
  }
  def messageCount = pages.size * messagesPerPage + uncompletedPage.size
  
  /** Messages are appended at the end, so the latest message appended means the oldest */
  def appendMessage(msg: Message): Unit = {
    uncompletedPage += msg
    if (uncompletedPage.size == messagesPerPage) {
      val baos = new ByteArrayOutputStream()
      val gzip = new GZIPOutputStream(baos)
      val output = new Output(gzip)
      kryo.writeObject(output, uncompletedPage.toArray)
      output.flush()
      gzip.close()
      pages += baos.toByteArray
      uncompletedPage.clear()
    }
  }
}
