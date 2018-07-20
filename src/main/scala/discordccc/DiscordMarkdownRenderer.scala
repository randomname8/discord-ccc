package discordccc

import ccc.{DefaultMarkdownNodeFactory, MarkdownRenderer}
import ccc.util.WeakImage
import javafx.application.HostServices
import javafx.scene.layout.Region
import javafx.scene.text.Font
import regex._

class DiscordMarkdownRenderer(hostServices: HostServices, imagesCache: collection.Map[String, WeakImage]) extends DefaultMarkdownNodeFactory(hostServices, imagesCache) {

  override def desiredImageHeight = Font.getDefault.getSize * 2.2
  override def mkCodeBlock(context: MarkdownRenderer.RenderContext)(language: Option[String], code: String) = {
    super.mkCodeBlock(context)(language, code.replace("\\\n", "\n"))
  }
  override def mkInlineContent(context: MarkdownRenderer.RenderContext)(title: String, url: String, altText: String) = {
    altText match {
      case "emoji" => super.mkEmoji(context)(title, imagesCache(url).get)
        
      case gr"""image$params( width=$width(\d+);height=$height(\d+))?""" =>
        val node = super.mkInlineContent(context)(title, url, title)
        params foreach (_ => node.asInstanceOf[Region].setPrefSize(width.get.toDouble, height.get.toDouble))
        node
        
      case _ => super.mkInlineContent(context)(title, url, altText)
    }
  }
}
