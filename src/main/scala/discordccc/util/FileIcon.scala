package discordccc.util

import better.files.File
import sys.process._

//utility to fetch a file icons
object FileIcon {

  private val isLinux = scala.util.Properties.isLinux
  
  def getIconUrlForFile(file: File): String = {
    if (isLinux) {
      val fileType = Seq("xdg-mime", "query", "filetype", file.toString).!!.trim
      getIconUrlForMimetype(fileType)
    } else "/generic-file.png"
  }
  def getIconUrlForMimetype(mimetype: String): String = {
    if (isLinux) {
      val baseFolder = File("/usr/share/icons/")
      val alternativeStyles = Seq(
        baseFolder/"oxygen/base/128x128/mimetypes")
      val iconFile = alternativeStyles.map(_/(mimetype.replace("/", "-") + ".png")).filter(_.exists).headOption
      iconFile.map("file:" + _) getOrElse "/generic-file.png"
    } else "/generic-file.png"
  }
}
