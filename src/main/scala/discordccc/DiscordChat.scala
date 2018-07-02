package discordccc

import better.files._
import ccc._, ccc.util._
import discordccc.model._
import java.time.{LocalDateTime, ZoneId}
import javafx.application.Application
import javafx.beans.property.SimpleObjectProperty
import javafx.fxml.FXMLLoader
import javafx.scene.control.{MenuBar, TreeView}
import javafx.scene.layout.Pane
import javafx.stage.Stage
import org.asynchttpclient.{DefaultAsyncHttpClient, DefaultAsyncHttpClientConfig}

object DiscordChat {
  def main(args: Array[String]): Unit = Application.launch(classOf[DiscordChat], args:_*)
}
class DiscordChat extends BaseApplication with NavigationTree {
  override val sceneRoot = FXMLLoader.load(getClass.getResource("/main-window.fxml")).asInstanceOf[Pane]
  val rootUserDataDirectory = (File.home/".discorccc").createDirectories()
  val emojioneDir = (rootUserDataDirectory/"emojione").createDirectories()
  val ahc = new DefaultAsyncHttpClient(new DefaultAsyncHttpClientConfig.Builder().setWebSocketMaxFrameSize(1024*1024).build())
  
  val emojis = EmojiOne.emojiLookup.map(e => e._1 -> new WeakImage(s"file:${emojioneDir}/${e._2.filename}.png"))
  val imagesCache: collection.mutable.Map[String, WeakImage] = new util.LruMap[String, WeakImage](100).withDefault { k => // cache the most recent images shown in the chat
    val res = new WeakImage(k)
    imagesCache(k) = res
    res
  }
  val markdownRenderer = new DiscordMarkdownRenderer(getHostServices, imagesCache)
  
  val chatModel: ChatModel = new CompressingChatModel()
  
  val chatList = new ChatList[Member, Message](getHostServices, markdownRenderer, emojis.mapValues(_.get),
                                               _.nickname,
                                               _.originalContent,
                                               m => LocalDateTime.ofInstant(m.created, ZoneId.systemDefault))
  
  chatList.messageFormatter set DiscordMarkdown.adaptToMarkdown
  chatList.additionalMessageRenderFactory set new AdditionalMessageRenderer(imagesCache, markdownRenderer)
  chatList.userNameNodeFactory set MemberRender
  
  
  lazy val menuBar = sceneRoot.lookup("#menubar").asInstanceOf[MenuBar]
  lazy val statusPanel = sceneRoot.lookup("#status-panel").asInstanceOf[Pane]
  lazy val navigationTree = sceneRoot.lookup("#accesstree").asInstanceOf[TreeView[Any]]
  
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
    
    discordLogin(stage)
  }
  
  private def discordLogin(stage: Stage): headache.DiscordClient = {
    val eventHandler = new DiscordEventHandler(this)
    val client = new DiscordLoginDialog(ahc, eventHandler).modify(_.initOwner(stage)).showAndWait()
    if (!client.isPresent) return sys.exit(0)
    client.get
  }
}