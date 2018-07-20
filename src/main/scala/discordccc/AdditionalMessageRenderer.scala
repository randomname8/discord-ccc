package discordccc

import ccc._, ccc.util._
import discordccc.model._
import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout._
import javafx.scene.text.Font

class AdditionalMessageRenderer(imageCache: collection.Map[String, WeakImage],
                                emojiProvider: Map[String, Image],
                                nodeFactory: DiscordMarkdownRenderer) extends ((Message, MarkdownRenderer.RenderContext) => Seq[Node]) {

  override def apply(message: Message, context: MarkdownRenderer.RenderContext) = {
    val attachmentNodes = message.attachments map {
      case Message.Attachment(fileName, url) if fileName.matches(".+(png|jpg|jpeg|gif|bmp|avi|flv|mkv|webm|mp4)") => 
        nodeFactory.mkInlineContent(context)(fileName, url, fileName)
      
      case Message.Attachment(fileName, url) if fileName matches ".+(mp3|ogg|flac|wav|opus)" =>
        val mediaPlayer = context.mediaPlayerProvider()
        mediaPlayer.setMaxHeight(Font.getDefault.getSize * 3)
        mediaPlayer.content set new Region().modify(_ setStyle "-fx-background-color: black")
        JfxUtils.showingProperty(mediaPlayer) foreach (showing => if (showing) {
            mediaPlayer.applyCss()
            mediaPlayer.getSkin.getNode.lookup(".media-overlay").visibleProperty.unbind()
            mediaPlayer.setMedia(url, None)
          })
        new nodeFactory.CollapsibleContent(fileName, mediaPlayer, url)
        
      case Message.Attachment(fileName, url) => nodeFactory.mkLink(context)(Some(fileName), url)
    }
    
    val richLayoutNodes = message.content.collect { case rl: Content.RichLayout => rl }.map(rl => renderRichLayout(rl, context).modify(_.getStyleClass.add("rich-layout-content")))
    richLayoutNodes ++ attachmentNodes 
  }
  
  def renderRichLayout(rl: Content.RichLayout, context: MarkdownRenderer.RenderContext): Node = {
    val gap = Font.getDefault.getSize / 2
    val titlePane = new HBox().modify(
      _.setSpacing(gap),
      _.setPrefHeight(Font.getDefault.getSize * 1.25),
      _.setAlignment(Pos.CENTER_LEFT))
    rl.color.foreach(c => titlePane.setStyle(f"-fx-background-color:  linear-gradient(to right, $c%06x, transparent)"))
    rl.author foreach { 
      case Content.Text(authorName) => titlePane.getChildren add new Label(authorName).modify(_.getStyleClass add "rich-layout-content-author")
      case author: Content.RichLayout =>
        author.image foreach (im => titlePane.getChildren add new ImageView(imageCache(im.url).get).modify(_ setFitHeight Font.getDefault.getSize * 2, _ setPreserveRatio true))
        (author.title, author.url) match {
          case (author, Some(u)) => titlePane.getChildren add nodeFactory.mkLink(context)(author, u).modify(_.getStyleClass add "rich-layout-content-author")
          case (Some(t), _) => titlePane.getChildren add new Label(t).modify(_.getStyleClass add "rich-layout-content-author")
          case _ =>
        }
      case other => throw new UnsupportedOperationException(s"$other not yet supported for titles")
    }

    val content = vbox()(spacing = gap)
    (rl.title, rl.url) match {
      case (title, Some(url)) => content.getChildren add nodeFactory.mkLink(context)(title, url).modify(_.getStyleClass add "rich-layout-content-title")
      case (Some(title), _) => content.getChildren add new Label(title).modify(_.getStyleClass add "rich-layout-content-title")
      case _ =>
    }
    rl.description foreach {
      case Content.Text(text) => MarkdownRenderer.render(text, emojiProvider, nodeFactory)(context) foreach content.getChildren.add
      case rl: Content.RichLayout => content.getChildren add renderRichLayout(rl, context)
      case other => throw new UnsupportedOperationException(s"$other not yet supported for description")
    }
    if (rl.fields.nonEmpty) {
      val rows = rl.fields.foldLeft(List.empty[Vector[Content.Field]]) {
        case (Nil, elem) => List(Vector(elem))
        case (acc, elem) if !elem.inline => Vector(elem) :: acc
        case (head :: rest, elem) => (head :+ elem) :: rest
      }.reverse //reverse needed due to how we process the rows using a List and the head

      
      val nodeRows = rows.map(_.flatMap(field => Seq(
            new Label(field.name).modify(_.getStyleClass add "rich-layout-content-field-key"),
            (field.value match {
                case Content.Text(t) => new Label(t)
                case rl: Content.RichLayout => renderRichLayout(rl, context)
                case other => throw new UnsupportedOperationException(s"$other not yet supported for field values")
              }).modify(_.getStyleClass add "rich-layout-content-field-value")
          )))
      
      content.getChildren add gridPane(nodeRows:_*)(vgap = gap, hgap = gap)
    }
    
    val thumbnail = rl.thumbnail map (tn => new ImageView(imageCache(tn.url).get).modify(
        _ setPreserveRatio true, _ setFitWidth tn.width.toDouble, _ setFitHeight tn.height.toDouble, BorderPane.setAlignment(_, Pos.TOP_CENTER)))
    
    val footer = rl.footer map { 
      case Content.Text(footer) => new Label(footer).modify(_.getStyleClass add "rich-layout-content-footer")
      case footer: Content.RichLayout =>
        val icon = footer.image map (im => new ImageView(imageCache(im.url).get).modify(_ setFitHeight Font.getDefault.getSize * 2, _ setPreserveRatio true))
        val text = footer.title map (t => new Label(t).modify(_.getStyleClass add "rich-layout-content-footer"))
        hbox(text.toSeq ++ icon:_*)(alignment = Pos.CENTER_RIGHT, spacing = gap)
      case other => throw new UnsupportedOperationException(s"$other not yet supported for footers")
    }
    
    val rootPane = new BorderPane()
    if (rl.color.isDefined || rl.author.isDefined) rootPane.setTop(titlePane)
    rootPane setCenter content
    thumbnail foreach rootPane.setRight
    footer foreach rootPane.setBottom
    rootPane
  }
}
