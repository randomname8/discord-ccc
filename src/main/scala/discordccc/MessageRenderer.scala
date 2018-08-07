package discordccc

import ccc._, ccc.util._
import discordccc.model._
import javafx.application.HostServices
import javafx.geometry.{Insets, Pos}
import javafx.scene.Node
import javafx.scene.Scene
import javafx.scene.control.Label
import javafx.scene.input.MouseButton
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout._
import javafx.scene.text.{Font, Text, TextFlow}
import javafx.scene.web.WebView
import javafx.stage.Stage

class MessageRenderer(hostServices: HostServices,
                      imageCache: collection.Map[String, WeakImage],
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
      case rl: Content.RichLayout => 
        val res = renderRichLayout(rl, context)
        res.getStyleClass.add("rich-layout-content")
//        res match { case r: Region => r.setMaxWidth(Region.USE_PREF_SIZE); case _ => }
        //wrap the rendered rich layout in a container that allows it to keep its preferred size
        StackPane.setAlignment(res, Pos.TOP_LEFT)
        Seq(new HBox(res))
      case Content.InlinedImage(name, url, _, true, w, h) => Seq(renderer.nodeFactory.mkEmoji(context)(name, emojiProvider(url)))
      case Content.InlinedImage(name, url, _, _, w, h) if w != -1 && h != -1 => Seq(renderer.nodeFactory.mkInlineContent(context)(name, url, name, w.toDouble, h.toDouble))
    }.flatten
    richLayoutNodes ++ attachmentNodes 
  }

  def renderMarkdown(text: String, context: MarkdownRenderer.RenderContext) =
    renderer.render(text.trim.replace(" ```", "\n```").replace("\n", "\\\n"), emojiProvider)(context)
  
  def renderRichLayout(rl: Content.RichLayout, context: MarkdownRenderer.RenderContext): Node = {
    val rootRichLayoutPane = new BorderPane()
    
    val gap = Font.getDefault.getSize / 2
    val titlePane = new HBox().modify { n =>
      n.setSpacing(gap)
      n.setPrefHeight(Font.getDefault.getSize * 1.25)
      n.setAlignment(Pos.CENTER_LEFT)
    }
    rl.color.foreach(c => titlePane.setStyle(f"-fx-background-color:  linear-gradient(to right, #$c%06x, transparent)"))
    rl.author foreach { 
      case Content.Text(authorName) => titlePane.getChildren add new Text(authorName).modify(_.getStyleClass add "rich-layout-content-author")
      case author: Content.RichLayout =>
        author.image foreach { im =>
          val view = new ImageView().modify(_ setFitHeight Font.getDefault.getSize * 2, _ setPreserveRatio true)
          titlePane.getChildren add view
          imageCache(im.url).onRetrieve(view.setImage)
        }
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
    
    def collapsibleImage(name: String, image: Content.InlinedImage) = renderer.nodeFactory.mkInlineContent(context)(name, image.url, "image", image.width.toDouble, image.height.toDouble)
    def renderSubContent(c: Content): Seq[Node] = c match {
      case Content.Text(text) => renderMarkdown(text, context)
      case image: Content.InlinedImage => Seq(collapsibleImage(image.name, image))
      case Content.InlinedMedia(name, url, _, true) =>
        val webview = new WebView()
        webview.setPrefSize(600, 480)
        webview.getEngine.load(url)
        rootRichLayoutPane.sceneProperty foreach (s =>  if (s == null) {println(s"unloading video $name - $url"); webview.getEngine.load("about:blank")}) //unload as soon as it exists a scene
        val node = new renderer.nodeFactory.CollapsibleContent(name, webview, url)
        node.setOnMouseClicked { evt => evt.getButton match {
            case MouseButton.SECONDARY =>
              val stage = new Stage()
              stage initOwner node.getScene.getWindow
              stage setTitle url
              node.setContent(null)
              stage setScene new Scene(webview)
              stage.sizeToScene()
              stage.setOnHidden { _ => 
                stage.getScene.setRoot(new javafx.scene.Group) //put something else there
                node.setContent(webview)
              }
              stage.show()
            case MouseButton.MIDDLE => hostServices.showDocument(url)
            case _ =>
          }}
        Seq(node)
      case rl: Content.RichLayout => Seq(renderRichLayout(rl, context))
      case other => throw new UnsupportedOperationException(s"$other not yet supported for description")
    }
    
    rl.description map renderSubContent foreach (s => content.getChildren.addAll(s:_*))
    
    if (rl.fields.nonEmpty) {
      val rows = rl.fields.foldLeft(List.empty[Vector[Content.Field]]) {
        case (Nil, elem) => List(Vector(elem))
        case (acc, elem) if !elem.inline => Vector(elem) :: acc
        case (head :: rest, elem) => (head :+ elem) :: rest
      }.reverse //reverse needed due to how we process the rows using a List and the head

      val grid = gridPane()(vgap = gap / 2, hgap = gap)
      
      for ((row, idx) <- rows.zipWithIndex) {
        grid.addRow(idx * 2, row.map(field => new Label(field.name).modify(_.getStyleClass add "rich-layout-content-field-key")):_*)
        grid.addRow(idx * 2 + 1, row.map(field => (renderSubContent(field.value) match {
                case elems if elems.size > 1 => vbox(elems:_*)(spacing = gap / 2, alignment = Pos.TOP_LEFT, fillWidth = true)
                case Seq(e) => e
              }).modify(_.getStyleClass add "rich-layout-content-field-value")):_*)
      }
      
      content.getChildren add grid
    }
    
    
    rl.image foreach(im => content.getChildren add collapsibleImage(im.name, im))
    
    val thumbnail = rl.thumbnail match {
      case Some(tn) if rl.image.isEmpty && (tn.width * tn.height > (150*150)) => 
        //when there's no main image and no video in the main content, make the thumbnail as main image if its size is large enough
        if (!rl.description.exists(_.isInstanceOf[Content.InlinedMedia]))
          content.getChildren add collapsibleImage(tn.name, tn)
        None
      case Some(tn) => Some(collapsibleImage("thumbnail", tn).modify(BorderPane.setAlignment(_, Pos.TOP_CENTER)))
      case None => None
    }
    
    val footer = rl.footer map {
      case Content.Text(footer) => new Label(footer).modify(_.getStyleClass add "rich-layout-content-footer")
      case footer: Content.RichLayout =>
        val icon = footer.image map { im => 
          val view = new ImageView().modify(_ setFitHeight Font.getDefault.getSize * 2, _ setPreserveRatio true)
          imageCache(im.url).onRetrieve(view.setImage)
          view
        }
        val text = footer.title map (t => new Label(t).modify(_.getStyleClass add "rich-layout-content-footer"))
        hbox(text.toSeq ++ icon:_*)(alignment = Pos.CENTER_RIGHT, spacing = gap)
      case other => throw new UnsupportedOperationException(s"$other not yet supported for footers")
    }
    
    if (rl.color.isDefined || rl.author.isDefined) rootRichLayoutPane.setTop(titlePane)
    rootRichLayoutPane setCenter content.modify(BorderPane.setMargin(_, new Insets(gap * 1.2)))
    thumbnail foreach rootRichLayoutPane.setRight
    footer foreach { f =>
      BorderPane.setMargin(f, new Insets(0.5.em, 0.5.em, 0.5.em, 1.5.em))
      rootRichLayoutPane.setBottom(f)
    }
    rootRichLayoutPane
  }
}
