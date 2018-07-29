package discordccc

import better.files._
import ccc._, ccc.util._
import discordccc.model._
import java.time.{LocalDateTime, ZoneId}
import javafx.application.Application
import javafx.beans.property.SimpleObjectProperty
import javafx.fxml.FXMLLoader
import javafx.scene.control.{MenuBar, TreeView, ScrollBar}
import javafx.scene.input.KeyCode
import javafx.scene.layout.{BorderPane, Pane}
import javafx.stage.Stage
import org.asynchttpclient.{DefaultAsyncHttpClient, DefaultAsyncHttpClientConfig}

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
  val imagesCache: collection.mutable.Map[String, WeakImage] = new LruMap[String, WeakImage](100).withDefault { k => // cache the most recent images shown in the chat
    val res = new WeakImage(k)
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
  chatList.messageRenderFactory set new MessageRenderer(imagesCache, emojis.mapValues(_.get), markdownRenderer)
  chatList.userNameNodeFactory set MemberRender
  
  val chatTextInput = new ChatTextInput(markdownRenderer, markdownRenderer.nodeFactory, emojis.mapValues(_.get))
  
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
    contentArea setCenter new BorderPane().modify(
      _ setCenter chatList,
      _ bottom chatTextInput)
    
    //configure the chat text input to only be enabled when there's a channel selected
    chatTextInput.textArea.disableProperty bind selectedMessageChannel.isNull
    
    chatTextInput.textArea.setOnKeyReleased { evt =>
      if (evt.getCode == KeyCode.ENTER) {
        if (evt.isShiftDown) {
          chatTextInput.textArea.insertText(chatTextInput.textArea.getCaretPosition, "\n")
        } else if (!evt.isControlDown && !evt.isAltDown) {
          evt.consume()
          sendUserInput()
        }
      }
    }


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
    val channel = selectedMessageChannel.get
    channel.connector.sendMessage(channel, Content.Text(content)).onComplete {
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
  
  private def discordLogin(stage: Stage): connector.DiscordConnector = {
    val eventHandler = new DiscordEventHandler(this)
    val client = new DiscordLoginDialog(ahc, eventHandler).modify(_.initOwner(stage)).showAndWait()
    if (!client.isPresent) return sys.exit(0)
    client.get
  }
}