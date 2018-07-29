package discordccc

import ccc._, ccc.util._
import discordccc.model._
import javafx.geometry.{Insets, Pos}
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout._
import javafx.scene.text.{Font, Text, TextFlow}

class MessageRenderer(imageCache: collection.Map[String, WeakImage],
                      emojiProvider: Map[String, Image],
                      renderer: DiscordMarkdownRenderer) extends ((Message, () => VlcMediaPlayer) => Seq[Node]) {

  override def apply(message: Message, vlcProvider: () => VlcMediaPlayer) = {
    val context = MarkdownRenderer.RenderContext(vlcProvider)
    val attachmentNodes = message.attachments map {
      case Message.Attachment(fileName, url) if fileName.toLowerCase.matches(".+(png|jpg|jpeg|gif|bmp|avi|flv|mkv|webm|mp4)") => 
        renderer.nodeFactory.mkInlineContent(context)(fileName, url, fileName)
      
      case Message.Attachment(fileName, url) if fileName.toLowerCase matches ".+(mp3|ogg|flac|wav|opus)" =>
        val mediaPlayer = context.mediaPlayerProvider()
        mediaPlayer.setMaxHeight(Font.getDefault.getSize * 3)
        mediaPlayer.content set new Region().modify(_ setStyle "-fx-background-color: black")
        JfxUtils.showingProperty(mediaPlayer) foreach (showing => if (showing) {
            mediaPlayer.applyCss()
            mediaPlayer.getSkin.getNode.lookup(".media-overlay").visibleProperty.unbind()
            mediaPlayer.setMedia(url, None)
          })
        new renderer.nodeFactory.CollapsibleContent(fileName, mediaPlayer, url)
        
      case Message.Attachment(fileName, url) => renderer.nodeFactory.mkLink(context)(Some(fileName), url)
    }
    
    val richLayoutNodes = message.content.collect { 
      case Content.Text(text) => renderMarkdown(text, context)
      case rl: Content.RichLayout => Seq(renderRichLayout(rl, context).modify(_.getStyleClass.add("rich-layout-content")))
      case Content.InlinedImage(name, url, _, true, w, h) => Seq(renderer.nodeFactory.mkEmoji(context)(name, emojiProvider(url)))
      case Content.InlinedImage(name, url, _, _, w, h) if w != -1 && h != -1 => Seq(renderer.nodeFactory.mkInlineContent(context)(name, url, name, w.toDouble, h.toDouble))
    }.flatten
    richLayoutNodes ++ attachmentNodes 
  }

  def renderMarkdown(text: String, context: MarkdownRenderer.RenderContext) =
    renderer.render(text.trim.replace(" ```", "\n```").replace("\n", "\\\n"), emojiProvider)(context)
  
  def renderRichLayout(rl: Content.RichLayout, context: MarkdownRenderer.RenderContext): Node = {
    val gap = Font.getDefault.getSize / 2
    val titlePane = new HBox().modify(
      _.setSpacing(gap),
      _.setPrefHeight(Font.getDefault.getSize * 1.25),
      _.setAlignment(Pos.CENTER_LEFT))
    rl.color.foreach(c => titlePane.setStyle(f"-fx-background-color:  linear-gradient(to right, #$c%06x, transparent)"))
    rl.author foreach { 
      case Content.Text(authorName) => titlePane.getChildren add new Text(authorName).modify(_.getStyleClass add "rich-layout-content-author")
      case author: Content.RichLayout =>
        author.image foreach (im => titlePane.getChildren add new ImageView(imageCache(im.url).get).modify(_ setFitHeight Font.getDefault.getSize * 2, _ setPreserveRatio true))
        (author.title, author.url) match {
          case (author, Some(u)) => titlePane.getChildren add renderer.nodeFactory.mkLink(context)(author, u).modify(_.getStyleClass add "rich-layout-content-author")
          case (Some(t), _) => titlePane.getChildren add new Text(t).modify(_.getStyleClass add "rich-layout-content-author")
          case _ =>
        }
      case other => throw new UnsupportedOperationException(s"$other not yet supported for titles")
    }

    val content = vbox()(spacing = gap, alignment = Pos.TOP_LEFT, fillWidth = true)
    (rl.title, rl.url) match {
      case (title, Some(url)) => content.getChildren add new TextFlow(renderer.nodeFactory.mkLink(context)(title, url).modify(_.getStyleClass add "rich-layout-content-title"))
      case (Some(title), _) => content.getChildren add new TextFlow(new Text(title).modify(_.getStyleClass add "rich-layout-content-title"))
      case _ =>
    }
    rl.description foreach {
      case Content.Text(text) => renderMarkdown(text, context) foreach content.getChildren.add
      case rl: Content.RichLayout => content.getChildren add renderRichLayout(rl, context)
      case other => throw new UnsupportedOperationException(s"$other not yet supported for description")
    }
    if (rl.fields.nonEmpty) {
      val rows = rl.fields.foldLeft(List.empty[Vector[Content.Field]]) {
        case (Nil, elem) => List(Vector(elem))
        case (acc, elem) if !elem.inline => Vector(elem) :: acc
        case (head :: rest, elem) => (head :+ elem) :: rest
      }.reverse //reverse needed due to how we process the rows using a List and the head

      val grid = gridPane()(vgap = gap / 2, hgap = gap)
      
      for ((row, idx) <- rows.zipWithIndex) {
        grid.addRow(idx * 2, row.map(field => new Label(field.name).modify(_.getStyleClass add "rich-layout-content-field-key")):_*)
        grid.addRow(idx * 2 + 1, row.map(field => (field.value match {
                case Content.Text(t) => vbox(renderMarkdown(t, context):_*)(spacing = gap / 2, alignment = Pos.TOP_LEFT, fillWidth = true)
                case rl: Content.RichLayout => renderRichLayout(rl, context)
                case other => throw new UnsupportedOperationException(s"$other not yet supported for field values")
              }).modify(_.getStyleClass add "rich-layout-content-field-value")):_*)
      }
      
      content.getChildren add grid
    }
    
    def collapsibleImage(name: String, image: Content.InlinedImage) = renderer.nodeFactory.mkInlineContent(context)(name, image.url, "image", image.width.toDouble, image.height.toDouble)
    
    rl.image foreach(im => content.getChildren add collapsibleImage(im.name, im))
    
    val thumbnail = rl.thumbnail match {
      case Some(tn) if rl.image.isEmpty && (tn.width * tn.height > (150*150)) => //when there's no main image, make the thumbnail as main image if its size is large enough
        content.getChildren add collapsibleImage(tn.name, tn)
        None
      case Some(tn) => Some(collapsibleImage("thumbnail", tn).modify(BorderPane.setAlignment(_, Pos.TOP_CENTER)))
      case None => None
    }
    
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
    rootPane setCenter content.modify(BorderPane.setMargin(_, new Insets(gap * 1.2)))
    thumbnail foreach rootPane.setRight
    footer foreach rootPane.setBottom
    rootPane
  }
}
