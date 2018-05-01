package discordccc

import better.files._
import ccc._
import java.time.LocalDateTime
import java.util.function.Consumer
import javafx.application.{Application, Platform}
import javafx.beans.property.SimpleObjectProperty
import javafx.fxml.FXMLLoader
import javafx.geometry._
import javafx.scene.Scene
import javafx.scene.control.{Label, ProgressBar, Alert, MenuBar, TreeView, ButtonType, ScrollBar}
import javafx.scene.input.KeyCode
import javafx.scene.layout.{BorderPane, Pane}
import javafx.stage.{Stage, StageStyle}
import net.dv8tion.jda.core.JDA
import net.dv8tion.jda.core.entities.{Message, User, MessageChannel}
import net.dv8tion.jda.core.events.Event
import net.dv8tion.jda.core.events.message.{MessageReceivedEvent, MessageUpdateEvent, MessageDeleteEvent, MessageEmbedEvent}
import okhttp3.OkHttpClient
import scala.collection.JavaConverters._

object CccDiscord extends App {
  Application.launch(classOf[CccDiscord], args:_*)
}
class CccDiscord extends BaseApplication with DiscordNavigationTree {
  override val sceneRoot = FXMLLoader.load(getClass.getResource("/main-window.fxml")).asInstanceOf[Pane]
  val rootUserDataDirectory = (File.home/".discorccc").createDirectories()
  val emojioneDir = (rootUserDataDirectory/"emojione").createDirectories()
  
  var discordClient: JDA = _
  
  val emojis = util.EmojiOne.emojiLookup.map(e => e._1 -> new util.WeakImage(s"file:${emojioneDir}/${e._2.filename}.png"))
  val imagesCache: collection.mutable.Map[String, util.WeakImage] = new util.LruMap[String, util.WeakImage](100).withDefault { k => // cache the most recent images shown in the chat
    val res = new util.WeakImage(k)
    imagesCache(k) = res
    res
  }  
  val markdownRenderer = new DiscordMarkdownRenderer(getHostServices, imagesCache)
  val chatList = new ChatList[Member, Message](getHostServices, markdownRenderer, emojis.mapValues(_.get),
                                             _.nickname,
                                             _.text,
                                             m => LocalDateTime.ofInstant(m.created, null))
  chatList.messageFormatter.set(DiscordMarkdown.adaptToMarkdown)
  chatList.additionalMessageRenderFactory set new DiscordAdditionalMessageRenderer(imagesCache, markdownRenderer)
//  chatList.userNameNodeFactory set DiscordMemberRender.apply
  val chatTextInput = new ChatTextInput(markdownRenderer, emojis.mapValues(_.get))
  
  lazy val menuBar = sceneRoot.lookup("#menubar").asInstanceOf[MenuBar]
  lazy val statusPanel = sceneRoot.lookup("#status-panel").asInstanceOf[Pane]
  lazy val accessTree = sceneRoot.lookup("#accesstree").asInstanceOf[TreeView[Any]]
  
  val selectedMessageChannel = new SimpleObjectProperty[MessageChannel](this, "selectedMessageChannel")
  
  override def extraInitialize(stage: Stage) = {
    if (emojioneDir.isEmpty() || { val subFiles = emojioneDir.list.map(_.nameWithoutExtension).toArray; !util.EmojiOne.allEmojis.forall(e => subFiles.contains(e.filename))}) {
      downloadEmojiOne()
    }
    stage.getScene.getStylesheets.addAll("/ccc-theme.css")
    stage.getScene.getStylesheets.addAll("/ccc-discord-theme.css")
    stage setTitle "CCC Discord"
    sceneRoot.applyCss() //we need the scene to be built in order for the lookups to work
    
    discordClient = discordLogin(stage)
    discordClient.addEventListener(discordListener)
    
    configureDiscordTree()
    
    val contentArea = sceneRoot.lookup("#content-area").asInstanceOf[BorderPane]
    
    contentArea setCenter new BorderPane().modify(
      _ setCenter chatList,
      _ bottom chatTextInput)
    
    //load 100 more items on scrolling to the top
    lazy val configureChatListScrollBar = {
      chatList.lookup(".scroll-bar:vertical").asInstanceOf[ScrollBar].valueProperty foreach { value =>
        if (value == 0 && chatList.itemsScala.nonEmpty) {
          selectedMessageChannel.get.getHistoryBefore(chatList.itemsScala.head.messages.head, 100).queue(
            history => Platform.runLater { () =>
              val prevItems = chatList.itemsScala.clone()
              chatList.itemsScala.clear()
              history.getRetrievedHistory.asScala.reverse.foreach (msg =>
                chatList.addEntry(msg.getAuthor, imagesCache(userImage(msg.getAuthor)), msg))
              chatList.itemsScala ++= prevItems
              prevItems.headOption foreach chatList.scrollTo
            }, jdaAsyncExceptionReporter)
        }
      }
    }
    
    //load the channel latest 100 items on selection
    selectedMessageChannel foreach {
      case null => chatList.itemsScala.clear()
      case mc =>
        println("retrieving messages since id " + mc.getLatestMessageIdLong)
        mc.getHistoryBefore(mc.getLatestMessageIdLong, 100).queue(
          history => Platform.runLater { () =>
            chatList.itemsScala.clear()
            history.getRetrievedHistory.asScala.reverse.foreach (msg =>
              chatList.addEntry(msg.getAuthor, imagesCache(userImage(msg.getAuthor)), msg))
            chatList.scrollTo(chatList.itemsScala.size)

            configureChatListScrollBar
          }, jdaAsyncExceptionReporter)
    }
    
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
  }
  
  def sendUserInput(): Unit = {
    val messageBuilder = new StringBuilder()
    val text = chatTextInput.textArea.getText.trim
           
    val usedEmojis = text.split("""\s+|(?!;_;|;P|;p|;\))[,;\.]""").flatMap(s => util.EmojiOne.emojiLookup.get(s).map(s -> _))
    var lastIdx = 0
    for ((toReplace, emoji) <- usedEmojis) {
      val idx = text.indexOf(toReplace, lastIdx) // because of our previous split and find, the index *has* to exists
      messageBuilder append text.substring(lastIdx, idx)
      messageBuilder append emoji.unicode
      lastIdx = idx + toReplace.length
    }
    if (lastIdx != text.length) messageBuilder append text.substring(lastIdx)
    val discordMsg = messageBuilder.result()
    chatTextInput.textArea.clear()
    selectedMessageChannel.get.sendMessage(discordMsg).queue()
  }
  def userImage(user: User): String = Option(user.getAvatarUrl).getOrElse(user.getDefaultAvatarUrl)
  
  object discordListener extends net.dv8tion.jda.core.hooks.EventListener {
    override def onEvent(event: Event): Unit = Platform.runLater { () =>
      event match {
        case evt: MessageReceivedEvent if evt.getChannel == selectedMessageChannel.get =>
          chatList.addEntry(evt.getAuthor, imagesCache(userImage(evt.getAuthor)), evt.getMessage)
        case evt: MessageUpdateEvent if evt.getChannel == selectedMessageChannel.get =>
          chatList.itemsScala.find(_.messages.exists(_.getIdLong == evt.getMessageIdLong)) foreach { box =>
            val idx = box.messages.indexWhere(_.getIdLong == evt.getMessageIdLong)
            val updated = box.copy(messages = box.messages.updated(idx, evt.getMessage))
            chatList.itemsScala.update(chatList.itemsScala indexOf box, updated)
          }
        case evt: MessageEmbedEvent if evt.getChannel == selectedMessageChannel.get =>
          chatList.itemsScala.find(_.messages.exists(_.getIdLong == evt.getMessageIdLong)) foreach { box =>
            val om = box.messages.find(_.getIdLong == evt.getMessageIdLong).get
            DiscordMessageUtils.setEmebds(om, evt.getMessageEmbeds)
            val idx = chatList.itemsScala indexOf box
            chatList.getItems.set(idx, box.copy(messages = Vector.empty)) //need to clean it first for the cell to update
            chatList.getItems.set(idx, box)
          }

        case evt: MessageDeleteEvent if evt.getChannel == selectedMessageChannel.get =>
          chatList.itemsScala.find(_.messages.exists(_.getIdLong == evt.getMessageIdLong)) foreach { box =>
            val updated = box.copy(messages = box.messages.filterNot(_.getIdLong == evt.getMessageIdLong))
            if (updated.messages.isEmpty) chatList.itemsScala -= box
            else chatList.itemsScala.update(chatList.itemsScala indexOf box, updated)
          }
        case _ =>
      }
    }
  }
  
  private def downloadEmojiOne(): Unit = {
    val loadingDialog = new Stage(StageStyle.UNDECORATED)
    val progressBar = new ProgressBar().modify(BorderPane.setMargin(_, new Insets(5)), _ setMaxWidth Double.MaxValue)
    val statusLabel = new Label("setting up...").modify(BorderPane.setMargin(_, new Insets(5)))
    loadingDialog setScene new Scene(new BorderPane().modify(_ setTop progressBar, _ setBottom statusLabel))
      
    new Thread(() => {
        val httpClient = new OkHttpClient()
        val response = httpClient.newCall(new okhttp3.Request.Builder().
                                          url("https://d1j8pt39hxlh3d.cloudfront.net/emoji/emojione/3.1.1/EmojiOne_3.1.1_128x128_png.zip").
                                          build()).execute()
        response.autoClosed foreach { response =>
          if (response.isSuccessful) {
            val zipFile = emojioneDir/"emojione.zip"
            zipFile.outputStream(File.OpenOptions.append) foreach { zipFile =>
              val byteStream = response.body.byteStream
              val totalBytes = response.body.contentLength
              val buffer = new Array[Byte](1024*4)
              var readSoFar = 0
              var in = 0
              while ({in = byteStream.read(buffer); in >= 0}) {
                zipFile.write(buffer, 0, in)
                readSoFar += in
                Platform.runLater { () => progressBar setProgress readSoFar / totalBytes.toDouble}
              }
            }
            zipFile.unzipTo(emojioneDir)
            zipFile.delete(true)
            Platform.runLater { () => loadingDialog.close() }
          } else {
            Platform.runLater { () =>
              loadingDialog.close()
              new Alert(Alert.AlertType.ERROR, "Failed downloading emojis " + response).showAndWait()
              sys.exit(1)
            }
          }
        }
      }).start()
      
    loadingDialog setWidth 400
    loadingDialog setHeight 300
    loadingDialog.centerOnScreen()
    loadingDialog.showAndWait()
  }
  
  private def discordLogin(stage: Stage): JDA = {
    val jda = new LoginDialog().modify(_.initOwner(stage)).showAndWait()
    if (!jda.isPresent) return sys.exit(0)
    jda.get
  }
  
  private val jdaAsyncExceptionReporter: Consumer[Throwable] = ex => Platform.runLater { () =>
    new Alert(Alert.AlertType.ERROR, ex.getMessage, ButtonType.OK).show()
  }
}
