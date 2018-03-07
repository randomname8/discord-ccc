package discordccc

import language.reflectiveCalls

import ccc._
import javafx.application.Application
import javafx.application.Platform
import javafx.scene.control.ScrollPane
import javafx.scene.control.TextInputDialog
import javafx.scene.input.KeyCode
import javafx.scene.layout.BorderPane
import javafx.scene.web.WebView
import javafx.stage.Stage
import net.dv8tion.jda.core.entities.{Guild, Message, User, PrivateChannel}
import net.dv8tion.jda.core.events.message.priv.{PrivateMessageReceivedEvent, PrivateMessageUpdateEvent}
import net.dv8tion.jda.core.events.message.{GenericMessageEvent, MessageReceivedEvent, MessageUpdateEvent}
import net.dv8tion.jda.core.events.{Event => JdaEvent}
import net.dv8tion.jda.core.hooks.{EventListener => JdaEventListener}
import net.dv8tion.jda.core.{AccountType, JDA, JDABuilder}
import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import uk.co.caprica.vlcj.component.DirectMediaPlayerComponent
import uk.co.caprica.vlcj.player.direct.DefaultDirectMediaPlayer
import uk.co.caprica.vlcj.player.direct.format.RV32BufferFormat

object DiscordClient extends App {
  System.setProperty("prism.lcdtext", "false")
  System.setProperty("prism.text", "t2k")
  Application.launch(classOf[DiscordClient], args:_*)
}
class DiscordClient extends BaseApplication {
  val emojis = util.EmojiOne.emojiLookup.map(e => e._1 -> new util.WeakImage(s"/emojione/128x128_png/${e._2.filename}.png"))
  private[this] val imagesCache: collection.mutable.Map[String, util.WeakImage] = new util.LruMap[String, util.WeakImage](100).withDefault { k => // cache the most recent images shown in the chat
    val res = new util.WeakImage(k)
    imagesCache(k) = res
    res
  }
  /**
   * cache some viewpanes, though only weakly, if they get claimed that's alright
   */
  private[this] val webViewCache = new util.WeakObjectPool[WebView](() => {
      val res = new WebView()
      res.contextMenuEnabled = false
      res.styleClass add "code-block"
      res
    })
  
  val markdownRenderer = new DefaultMarkdownNodeFactory(getHostServices, imagesCache)
  val chatList = new ChatList[User, Message](markdownRenderer, webViewCache, emojis.mapValues(_.get),
                                             _.getName,
                                             renderDiscordMessage,
                                             m => m.getCreationTime.toLocalDateTime)
  val chatTextInput = new ChatTextInput(markdownRenderer, webViewCache, emojis.mapValues(_.get))
  val sceneRoot = new BorderPane {
    this center new ScrollPane(chatList).modify(_.fitToWidth = true, _.fitToHeight = true)
    this bottom chatTextInput
  }
  
  private def renderDiscordMessage(message: Message): String = {
    var text = message.getContentDisplay
    if (!message.getAttachments.isEmpty) message.getAttachments forEach { a =>
      if (a.getFileName.split("\\.").last matches "jpe?g|png|gif")
        text += "\n" + s"""![${a.getFileName}](${a.getUrl} "${a.getFileName}")"""
      else
        text += "\n" + s"""[${a.getFileName}](${a.getUrl} "${a.getFileName}")"""
    }
    DiscordMarkdown adaptToMarkdown text
  }
  val mediaPlayerComponent = new DirectMediaPlayerComponent(new RV32BufferFormat(_, _))
  chatList.additionalMessageRenderFactory set { (message, maxWidth) => 
    message.getAttachments.asScala.filter(_.getFileName.split("\\.").last matches "avi|flv|webm|mp4|mkv") map { attachment =>
      new javafx.scene.control.Button("Play " + attachment.getFileName).modify(
        _.onAction = { _ =>
          val stage = new Stage()
          stage.title = "Playing " + attachment.getFileName
          stage.width = 400
          stage.height = 400
          
          val renderer = new util.TextureNode(new util.VlcTexture(mediaPlayerComponent.getMediaPlayer.asInstanceOf[DefaultDirectMediaPlayer]), 20)
          
          stage.scene = new javafx.scene.Scene(renderer)
          
          // configure the renderer so that it only plays when this node is visible
          util.JfxUtils.showingProperty(renderer).foreach { showing =>
            if (showing) renderer.renderer.start() 
            else renderer.renderer.stop()
          }
  
          stage.show()
          mediaPlayerComponent.getMediaPlayer.playMedia(attachment.getUrl)
        })
    }
  }
  
  var discordClient: JDA = _
  override def extraInitialize(stage: Stage): Unit = {
    stage.title = "CCC-discard"
    stage.width = 1080
    stage.height = 720
    stage.scene.stylesheets.add("/ccc-theme.css")
    
    val tokenInput = new TextInputDialog().modify(
      _.title = "Connect to discord",
      _.headerText = "Enter user token")
    
    val tokenOption = tokenInput.showAndWait()
    if (!tokenOption.isPresent) sys.exit(0)
    
    discordClient = new JDABuilder(AccountType.CLIENT).setToken(tokenOption.get).buildBlocking()
    val selfUser = discordClient.getSelfUser
    
    val targetUser = 213062693496553473l
    val textChannel = discordClient.getTextChannelById(307260368764534784l)
//    val textChannel = discordClient.getUserById(targetUser).openPrivateChannel().complete()
    
    textChannel.getHistoryBefore(textChannel.getLatestMessageIdLong, 100).complete().getRetrievedHistory.asScala.reverse.foreach { msg =>
      chatList.addEntry(msg.getAuthor, imagesCache(msg.getAuthor.getAvatarUrl), msg)
    }
    
    discordClient addEventListener new JdaEventListener {
      override def onEvent(event: JdaEvent): Unit = Platform runLater {() =>
        try {
          event match {
            case msgEvt: MessageReceivedEvent if msgEvt.getTextChannel == textChannel => 
              chatList.addEntry(msgEvt.getAuthor, imagesCache(msgEvt.getAuthor.getAvatarUrl), msgEvt.getMessage)
            case msgEvt: MessageUpdateEvent  if msgEvt.getTextChannel == textChannel =>
              chatList.itemsScala.find(_.messages.exists(_.getIdLong == msgEvt.getMessageIdLong)) foreach { msgBox =>
                val updatedMessages = msgBox.messages map (m => if (m.getIdLong == msgEvt.getMessageIdLong) msgEvt.getMessage else m)
                val idx = chatList.itemsScala.indexOf(msgBox)
                chatList.itemsScala(idx) = msgBox.copy(messages = updatedMessages)
              }
              
//            case msgEvt: PrivateMessageReceivedEvent if msgEvt.getAuthor.getIdLong == targetUser =>
//              chatList.addEntry(msgEvt.getAuthor, imagesCache(msgEvt.getAuthor.getAvatarUrl), msgEvt.getMessage)
//            case msgEvt: PrivateMessageUpdateEvent  if msgEvt.getAuthor.getIdLong == targetUser =>
//              chatList.itemsScala.find(_.messages.exists(_.getIdLong == msgEvt.getMessageIdLong)) foreach { msgBox =>
//                val updatedMessages = msgBox.messages map (m => if (m.getIdLong == msgEvt.getMessageIdLong) msgEvt.getMessage else m)
//                val idx = chatList.itemsScala.indexOf(msgBox)
//                chatList.itemsScala(idx) = msgBox.copy(messages = updatedMessages)
//              }
//            
            case _ =>
          }
        } catch {
          case NonFatal(e) => e.printStackTrace()
        }
      }
    }
    
    chatTextInput.textArea.onKeyReleased = evt => {
      if (evt.getCode == KeyCode.ENTER) {
        if (evt.isShiftDown) {
          chatTextInput.textArea.insertText(chatTextInput.textArea.getCaretPosition, "\n")
        } else if (!evt.isControlDown && !evt.isAltDown) {
          evt.consume()
          val msg = chatTextInput.textArea.text.trim
          chatTextInput.textArea.clear()
          if (textChannel.isInstanceOf[PrivateChannel]) {
            textChannel.sendMessage(msg).queue(msg => Platform.runLater(() => 
                chatList.addEntry(selfUser, new util.WeakImage(selfUser.getAvatarUrl), msg)))
          } else textChannel.sendMessage(msg).queue()
        }
      }
    }
    
  }
  
  type MessageEvent = GenericMessageEvent {
    def getAuthor(): User
    def getMessage(): Message
    def getGuild(): Guild
  }
  object MessageEvent {
    def unapply(evt: GenericMessageEvent) = evt match {
      case _: MessageReceivedEvent | _: MessageUpdateEvent => Some(evt.asInstanceOf[MessageEvent])
      case _ => None
    }
  }
}
