package discordccc

import ccc.DefaultMarkdownNodeFactory
import ccc.util.WeakImage
import javafx.application.HostServices
import javafx.scene.text.Font

class DiscordMarkdownRenderer(hostServices: HostServices, imagesCache: collection.Map[String, WeakImage]) extends DefaultMarkdownNodeFactory(hostServices, imagesCache) {

  override def desiredImageHeight = Font.getDefault.getSize * 2.2
  override def mkCodeBlock(context)(language: Option[String], code: String) = {
    super.mkCodeBlock(context)(language, code.replace("\\\n", "\n"))
  }
  override def mkInlineContent(context)(title, url, altText) = {
    if (altText == "emoji") super.mkEmoji(context)(title, imagesCache(url).get)
    else super.mkInlineContent(context)(title, url, altText)
  }
}
