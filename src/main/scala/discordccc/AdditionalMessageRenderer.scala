package discordccc

import ccc._
import javafx.scene.Node
import javafx.scene.control.Tooltip
import javafx.scene.layout.Region
import javafx.scene.text.Font
import javafx.scene.web.WebView
import net.dv8tion.jda.core.entities.{EmbedType, Message}
import scala.collection.JavaConverters._

class DiscordAdditionalMessageRenderer(imageCache: collection.Map[String, util.WeakImage],
                                       nodeFactory: DiscordMarkdownRenderer) extends ((Message, MarkdownRenderer.RenderContext) => Seq[Node]) {

  override def apply(message: Message, context: MarkdownRenderer.RenderContext) = {
    val attachmentNodes = message.getAttachments.asScala map {
      case a if a.getFileName.matches(".+(png|jpg|jpeg|gif|bmp|avi|flv|mkv|webm|mp4)") => 
        nodeFactory.mkInlineContent(context)(a.getFileName, a.getUrl, a.getFileName)
      
      case a if a.getFileName matches ".+(mp3|ogg|flac|wav|opus)" =>
        val mediaPlayer = context.mediaPlayerProvider()
        mediaPlayer.setMaxHeight(Font.getDefault.getSize * 3)
        mediaPlayer.content set new Region().modify(_ setStyle "-fx-background-color: black")
        util.JfxUtils.showingProperty(mediaPlayer) foreach (showing => if (showing) {
            mediaPlayer.applyCss()
            mediaPlayer.getSkin.getNode.lookup(".media-overlay").visibleProperty.unbind()
            mediaPlayer.setMedia(a.getUrl, None)
          })
        new nodeFactory.CollapsibleContent(a.getFileName, mediaPlayer, a.getUrl)
        
      case a => nodeFactory.mkLink(context)(Some(a.getFileName), a.getUrl)
    }
    val embeds = message.getEmbeds.asScala flatMap { embed =>
      embed.getType match {
        case EmbedType.VIDEO => 
          val webView = new WebView()
          webView.getEngine.load(embed.getVideoInfo.getUrl)
          webView.setPrefHeight(500)
          Some(new nodeFactory.CollapsibleContent(embed.getTitle, webView, embed.getUrl).modify(
              _.setTooltip(new Tooltip(embed.getDescription))))
        case EmbedType.IMAGE => Some(nodeFactory.mkInlineContent(context)(
              Seq(embed.getTitle, embed.getDescription, embed.getUrl).find(_ != null).getOrElse("Unk."),
              embed.getThumbnail.getUrl, embed.getDescription))
        case EmbedType.RICH => Some(nodeFactory.mkCodeBlock(context)(Some("json"), embed.toJSONObject.toString))
        case _ => None
      }
    }
    attachmentNodes ++ embeds
  }
}
