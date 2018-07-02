package discordccc
package util

import better.files.InputStreamOps
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPOutputStream}

/**
 * Compresses storage in sequential fashion.
 * 
 * This storage allows storage and iteration of the elements in the same order they were stored but keeps them internally compressed.
 */
final class CompressedSequentialStorage[T](val kryo: KryoPool, val elementsPerPage: Int) {
  /**
   * Represents the current uncompleted page. Should always be the oldest messages.
   */
  private[this] val uncompletedPage = new collection.mutable.ArrayBuffer[T](elementsPerPage)
  private[this] val pages = new collection.mutable.ArrayBuffer[Array[Byte]](10)
  
  /** Retrieves page `latest - i`, i = 0 means the most recent elements.
   *  A page has at most `messagePerPage` entries.
   */
  def page(i: Int): Seq[T] = {
    if (pages.size == i) uncompletedPage.to[Vector]
    else if (pages.size > i) {
      kryo.fromBytes(new ByteArrayInputStream(pages(i)).asGzipInputStream().readAllBytes(), classOf[Vector[T]])
    }
    else Seq.empty
  }
  def count = pages.size * elementsPerPage + uncompletedPage.size
  
  /** Messages are appended at the end, so the latest message appended means the oldest */
  def append(elem: T): Unit = synchronized {
    uncompletedPage += elem
    if (uncompletedPage.size == elementsPerPage) {
      val baos = new ByteArrayOutputStream()
      val gzip = new GZIPOutputStream(baos)
      gzip.write(kryo.toBytesWithoutClass(uncompletedPage.to[Vector]))
      gzip.close()
      pages += baos.toByteArray
      uncompletedPage.clear()
    }
  }
  
  def iterator: Iterator[T] = (0 to pages.size).iterator flatMap page
}
