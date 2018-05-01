package discordccc

import ccc._
import javafx.scene.Node
import javafx.scene.layout.Region
import javafx.scene.text.Font

class AdditionalMessageRenderer(imageCache: collection.Map[String, util.WeakImage],
                                nodeFactory: DiscordMarkdownRenderer) extends ((Message, MarkdownRenderer.RenderContext) => Seq[Node]) {

  override def apply(message: Message, context: MarkdownRenderer.RenderContext) = {
    val attachmentNodes = message.attachments map {
      case Message.Attachment(fileName, url) if fileName.matches(".+(png|jpg|jpeg|gif|bmp|avi|flv|mkv|webm|mp4)") => 
        nodeFactory.mkInlineContent(context)(fileName, url, fileName)
      
      case Message.Attachment(fileName, url) if fileName matches ".+(mp3|ogg|flac|wav|opus)" =>
        val mediaPlayer = context.mediaPlayerProvider()
        mediaPlayer.setMaxHeight(Font.getDefault.getSize * 3)
        mediaPlayer.content set new Region().modify(_ setStyle "-fx-background-color: black")
        util.JfxUtils.showingProperty(mediaPlayer) foreach (showing => if (showing) {
            mediaPlayer.applyCss()
            mediaPlayer.getSkin.getNode.lookup(".media-overlay").visibleProperty.unbind()
            mediaPlayer.setMedia(url, None)
          })
        new nodeFactory.CollapsibleContent(fileName, mediaPlayer, url)
        
      case Message.Attachment(fileName, url) => nodeFactory.mkLink(context)(Some(fileName), url)
    }
//    val attachmentNodes = message.getAttachments.asScala map {
//    }
//    val embeds = message.getEmbeds.asScala flatMap { embed =>
//      embed.getType match {
//        case EmbedType.VIDEO => 
//          val webView = new WebView()
//          webView.getEngine.load(embed.getVideoInfo.getUrl)
//          webView.setPrefHeight(500)
//          Some(new nodeFactory.CollapsibleContent(embed.getTitle, webView, embed.getUrl).modify(
//              _.setTooltip(new Tooltip(embed.getDescription))))
//        case EmbedType.IMAGE => Some(nodeFactory.mkInlineContent(context)(
//              Seq(embed.getTitle, embed.getDescription, embed.getUrl).find(_ != null).getOrElse("Unk."),
//              embed.getThumbnail.getUrl, embed.getDescription))
//        case EmbedType.RICH => Some(nodeFactory.mkCodeBlock(context)(Some("json"), embed.toJSONObject.toString))
//        case _ => None
//      }
    attachmentNodes 
  }
}
