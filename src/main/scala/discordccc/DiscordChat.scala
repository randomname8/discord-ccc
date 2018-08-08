package discordccc

import better.files._
import ccc._, ccc.util._
import discordccc.model._
import discordccc.util.{AsyncWeakImageFactory, FileIcon}
import java.time.{LocalDateTime, ZoneId}
import javafx.application.Application
import javafx.beans.property.SimpleObjectProperty
import javafx.collections.ListChangeListener
import javafx.fxml.FXMLLoader
import javafx.geometry.{Insets, Orientation}
import javafx.scene.Scene
import javafx.scene.control.{ContentDisplay, MenuBar, TreeView, ScrollBar, Button, ListView, ListCell}
import javafx.scene.image.Image
import javafx.scene.image.ImageView
import javafx.scene.input.{Clipboard, KeyCode, KeyCombination, KeyEvent, DataFormat}
import javafx.scene.layout.{BorderPane, Pane}
import javafx.stage.{Modality, Stage}
import org.asynchttpclient.request.body.multipart.{ByteArrayPart, FilePart}
import org.asynchttpclient.{DefaultAsyncHttpClient, DefaultAsyncHttpClientConfig}
import scala.collection.JavaConverters._

object DiscordChat {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[DiscordChat], args:_*)
  }
}
class DiscordChat extends BaseApplication with NavigationTree with ConnectorListener {
  override val sceneRoot = FXMLLoader.load(getClass.getResource("/main-window.fxml")).asInstanceOf[Pane]
  val rootUserDataDirectory = (File.home/".discorccc").createDirectories()
  val emojioneDir = (rootUserDataDirectory/"emojione").createDirectories()
  val ahc = new DefaultAsyncHttpClient(new DefaultAsyncHttpClientConfig.Builder().setWebSocketMaxFrameSize(1024*1024).build())
  
  val emojis = EmojiOne.emojiLookup.map(e => e._1 -> new WeakImage(s"file:${emojioneDir}/${e._2.filename}.png"))
  val emojisLookup = emojis.mapValues(_.get.value.get.get)
  val imageFactory = new AsyncWeakImageFactory(ahc)
  val imagesCache: collection.mutable.Map[String, WeakImage] = new LruMap[String, WeakImage](100).withDefault { k => // cache the most recent images shown in the chat
    val res = imageFactory.asyncWeakImage(k)
    imagesCache(k) = res
    res
  }
  val markdownRenderer = new DiscordMarkdownRenderer(getHostServices, imagesCache,
                                                     id => Option(selectedMessageChannel.get).flatMap(c => c.connector.getUser(id)),
                                                     id => Option(selectedMessageChannel.get).flatMap(c => c.connector.getChannel(id)),
                                                     id => Option(selectedMessageChannel.get).flatMap(c => c.connector.getCustomEmoji(id)))
  val chatList = new ChatList[Member, Message](getHostServices,
                                               _.nickname,
                                               _.originalContent,
                                               m => LocalDateTime.ofInstant(m.created, ZoneId.systemDefault))
  chatList.messageRenderFactory set new MessageRenderer(getHostServices, imagesCache, emojisLookup, markdownRenderer)
  chatList.userNameNodeFactory set MemberRender
  chatList.messageControlsFactory set MessageExtraControls
  
  val chatTextInput = new ChatTextInput(markdownRenderer, markdownRenderer.nodeFactory, emojisLookup)
  case class Attachment(name: String, mimeType: String, image: Image, content: Any)
  val attachments = new ListView[Attachment]()
  
  lazy val menuBar = sceneRoot.lookup("#menubar").asInstanceOf[MenuBar]
  lazy val statusPanel = sceneRoot.lookup("#status-panel").asInstanceOf[Pane]
  lazy val navigationTree = sceneRoot.lookup("#accesstree").asInstanceOf[TreeView[Any]]
  lazy val contentArea = sceneRoot.lookup("#content-area").asInstanceOf[BorderPane]
  
  val selectedMessageChannel = new SimpleObjectProperty[Channel](this, "selectedMessageChannel")
  
  
  override def extraInitialize(stage: Stage) = {
    sys.props("org.slf4j.simpleLogger.defaultLogLevel") = "debug"
    if (emojioneDir.isEmpty() || { val subFiles = emojioneDir.list.map(_.nameWithoutExtension).toArray; !EmojiOne.allEmojis.forall(e => subFiles.contains(e.filename))}) {
      EmojiOneRetriever.downloadEmojiOne(ahc, emojioneDir)
    }
    stage.getScene.getStylesheets.addAll("/ccc-theme.css")
    stage.getScene.getStylesheets.addAll("/ccc-discord-theme.css")
    stage setTitle "CCC Discord"
    sceneRoot.applyCss() //we need the scene to be built in order for the lookups to work
    
    configureNavigationTree()
    
    
    val richLayoutDialog = new Stage()
    richLayoutDialog.initOwner(stage)
    richLayoutDialog.initModality(Modality.NONE)
    val richLayoutPane = new RichLayoutBuilderPane(new ChatTextInput(markdownRenderer, markdownRenderer.nodeFactory, emojisLookup))
    richLayoutDialog.setScene(new Scene(richLayoutPane).modify(_.getStylesheets.addAll(stage.getScene.getStylesheets)))
    richLayoutDialog.sizeToScene()
    
    val showRichLayoutDialog = new Button("Rich Layout")
    showRichLayoutDialog.setOnAction(_ => richLayoutDialog.show())
    
    richLayoutPane.onSubmit.set { rl => 
      val channel = selectedMessageChannel.get
      channel.connector.sendMessage(channel, rl).onComplete {
        case scala.util.Success(_) => 
        case scala.util.Failure(ex) =>
          ex.printStackTrace
      }(JavafxExecutionContext)
    }
    
    val textInputArea = new BorderPane()
    textInputArea.setCenter(chatTextInput)
    BorderPane.setMargin(showRichLayoutDialog, new Insets(0, 0, 0, 5))
    textInputArea.setRight(showRichLayoutDialog)
    textInputArea.setBottom(attachments)
    
    contentArea setCenter new BorderPane().modify(
      _ setCenter chatList,
      _ bottom textInputArea)
    
    //configure the chat text input to only be enabled when there's a channel selected
    chatTextInput.textArea.disableProperty bind selectedMessageChannel.isNull
    showRichLayoutDialog.disableProperty bind selectedMessageChannel.isNull
    
    chatTextInput.textArea.setOnKeyReleased { evt =>
      if (evt.getCode == KeyCode.ENTER) {
        val ta = chatTextInput.textArea
        if (evt.isShiftDown) {
          ta.insertText(ta.getCaretPosition, "\n")
        } else if (!evt.isControlDown && !evt.isAltDown && ta.getCaretPosition == ta.getLength) {
          evt.consume()
          sendUserInput()
        }
      }
    }
    
    //setup attachment support
    setupAttachments()


    //load 100 more items on scrolling to the top
    lazy val configureChatListScrollBar = {
      chatList.lookup(".scroll-bar:vertical").asInstanceOf[ScrollBar].valueProperty foreach { value =>
        if (value == 0 && chatList.itemsScala.nonEmpty) {
          val channel = selectedMessageChannel.get
          channel.connector.getLastMessages(channel, Some(chatList.itemsScala.head.messages.head.id), Some(100)).onComplete {
            case scala.util.Success(msgs) => 
              val prevItems = chatList.itemsScala.clone()
              chatList.itemsScala.clear()
              msgs foreach addMessage
              chatList.scrollTo(chatList.itemsScala.size)
              chatList.itemsScala ++= prevItems
              prevItems.headOption foreach chatList.scrollTo
            case scala.util.Failure(ex) =>
              ex.printStackTrace
          }(JavafxExecutionContext)
        }
      }
    }
    
    selectedMessageChannel.foreach { c => 
      chatList.itemsScala.clear()
      if (c != null) {
        c.connector.getLastMessages(c).onComplete {
          case scala.util.Success(msgs) => 
            msgs foreach addMessage
            chatList.scrollTo(chatList.itemsScala.size)
            
            configureChatListScrollBar //can only configure this now
            
            msgs.lastOption foreach (c.connector.markAsRead(_).failed.foreach(_.printStackTrace)(JavafxExecutionContext))
          case scala.util.Failure(ex) =>
            ex.printStackTrace
        }(JavafxExecutionContext)
      }
    }
    
    discordLogin(stage).listeners += connectorListener
    
    val darkTheme = File("dark-theme.css")
    stage.getScene.getStylesheets.add(darkTheme.url.toString)
    
    
    new FileMonitor(File("dark-theme.css")) {
      override def onModify(file: File, count: Int) = JavafxExecutionContext.execute { () =>
        println("reloading css")
        stage.getScene.getStylesheets.remove(darkTheme.url.toString)
        stage.getScene.getStylesheets.add(darkTheme.url.toString)
      }
    }.start()(scala.concurrent.ExecutionContext.fromExecutorService(java.util.concurrent.Executors.newSingleThreadExecutor()))
  }
  
  private def sendUserInput(): Unit = {
    val messageBuilder = new StringBuilder()
    val text = chatTextInput.textArea.getText.trim
           
    val usedEmojis = text.split("""\s+|(?!;_;|;P|;p|;\))[,;\.]""").flatMap(s => EmojiOne.emojiLookup.get(s).map(s -> _))
    var lastIdx = 0
    for ((toReplace, emoji) <- usedEmojis) {
      val idx = text.indexOf(toReplace, lastIdx) // because of our previous split and find, the index *has* to exists
      messageBuilder append text.substring(lastIdx, idx)
      messageBuilder append emoji.unicode
      lastIdx = idx + toReplace.length
    }
    if (lastIdx != text.length) messageBuilder append text.substring(lastIdx)
    val content = messageBuilder.result()
    chatTextInput.textArea.clear()
    
    val repeatedNames = collection.mutable.Map[String, Int]().withDefaultValue(0)
    def adaptName(name: String): String = {
      repeatedNames.get(name) match {
        case Some(counter) =>
          repeatedNames(name) = counter + 1
          (counter + 1) + name
        case _ => 
          repeatedNames(name) = 1
          name
      }
    }
    val parts = attachments.getItems.asScala.zipWithIndex.collect {
      case (Attachment(name, mimeType, _, f: java.io.File), idx) => new FilePart("part" + idx, f, mimeType, "utf-8", adaptName(f.getName))
      case (Attachment(name, mimeType, _, buffer: java.nio.ByteBuffer), idx) => new ByteArrayPart("part" + idx, buffer.array, mimeType, "utf-8", adaptName(name))
    }
    attachments.getItems.clear()
    
    val channel = selectedMessageChannel.get
    channel.connector.sendMessage(channel, Content.Text(content), attachments = parts).onComplete {
      case scala.util.Success(msg) =>
      case scala.util.Failure(ex) =>
        ex.printStackTrace
    }(JavafxExecutionContext)
  }
  
  protected def addMessage(message: Message): Unit = {
    val selectedChannel = selectedMessageChannel.get
    val connector = selectedChannel.connector
    val user = connector.getUser(message.authorId).getOrElse(User(0, "unk.", false, "non existent user?", None, false, connector))
    val member = connector.getMember(message.authorId, message.channelId).getOrElse( 
      Member(user.id, selectedChannel.serverId.getOrElse(0), user.name, Seq.empty, 0, false, connector))
    chatList.addEntry(member, imagesCache(user.imageUrl.getOrElse("/red-questionmark.png")), message)
  }
  protected def updateMessage(message: MessageUpdate): Unit = {
    chatList.itemsScala.zipWithIndex.find(_._1.messages.exists(_.id == message.id)) foreach { case (box, idx) =>
        val newBox = box.copy(messages = box.messages.map { 
            case m if m.id == message.id => m.copy(
                content = message.content.getOrElse(m.content),
                edited = message.edited,
                attachments = message.attachments.getOrElse(m.attachments),
              )
            case m => m
          }) 
        chatList.itemsScala(idx) = newBox
    }
  }
  
  private def discordLogin(stage: Stage): connector.DiscordConnector = {
    val client = new DiscordLoginDialog(ahc).modify(_.initOwner(stage)).showAndWait()
    if (!client.isPresent) return sys.exit(0)
    client.get
  }
  
  private def setupAttachments(): Unit = {
    attachments.setMinHeight(0)
    attachments.setMaxHeight(0)
    val previewSize = 128d
    attachments.getItems.addListener({ evt => 
        if (evt.getList.isEmpty) attachments.setMaxHeight(0) 
        else if (attachments.getMaxHeight == 0) attachments.setMaxHeight(previewSize + 2.5.em) 
      }: ListChangeListener[Attachment])
    attachments.setOrientation(Orientation.HORIZONTAL)
    
    attachments.setCellFactory(_ => new ListCell[Attachment] {
        setContentDisplay(ContentDisplay.BOTTOM)
        setOnMouseClicked { evt =>
          val item = getItem()
          if (item != null) attachments.getItems.remove(item)
        }
        setBackground(null)
        override protected def updateItem(item: Attachment, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          if (item != null && !empty) {
            val image = item.image
            val imageView = new ImageView(image)
            imageView.setPreserveRatio(true)
            if (image.getWidth > image.getHeight) imageView.setFitWidth(previewSize) else imageView.setFitHeight(previewSize)
            setGraphic(imageView)
            setText(item.name)
          } else {
            setText(null)
            setGraphic(null)
          }
        }
      })
    
    val pasteCombination = KeyCombination.keyCombination("ctrl+v")
    val pngDataFormat = new DataFormat("image/png")
    val clipboard = Clipboard.getSystemClipboard()
    chatTextInput.textArea.addEventFilter[KeyEvent](KeyEvent.KEY_PRESSED, evt => {
        if (pasteCombination.`match`(evt)) {
          if (clipboard.hasImage) {
            val image = clipboard.getImage()
            val png = clipboard.getContent(pngDataFormat)
            attachments.getItems.add(Attachment("image.png", "image/png", image, png))
            evt.consume()
          } else if (clipboard.hasFiles) {
            clipboard.getFiles forEach { file =>
              val image = FileIcon.getIconUrlForFile(file.toScala)
              println(file + " → icon " + image)
              attachments.getItems.add(Attachment(file.getName, "application/octet-stream", new Image(image), file))
            }
            evt.consume()
          }
        }
      })
  }
}