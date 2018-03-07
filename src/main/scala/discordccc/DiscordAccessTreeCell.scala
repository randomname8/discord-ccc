package discordccc

import ccc._
import javafx.beans.value.WeakChangeListener
import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.control.skin.VirtualFlow
import javafx.scene.control.{Label, ListCell, ListView, TitledPane, TreeCell, TreeItem}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.text.Font
import net.dv8tion.jda.client.entities.{Group => GroupChannel}
import net.dv8tion.jda.core.entities.{Member, PrivateChannel, TextChannel, User}
import scala.collection.JavaConverters._

object DiscordAccessTreeCell {
  val textLabelFont = Font.font(Font.getDefault.getName, Font.getDefault.getSize * 1.5)
  val unreadableChannelColor = Color.RED
}
class DiscordAccessTreeCell(
  val imagesCache: collection.Map[String, util.WeakImage],
  val guildOwnerIcon: Image) extends TreeCell[Any] {
  import DiscordAccessTreeCell._, JavafxJdaWrapper._

  //strong reference to our listener, this changes per item, effectively discarding the previous listener, and hence allowing it to be 
  //reclaimed
  private var eventListenerReference: WeakChangeListener[java.lang.Boolean] = _
  private def newEventListenerReference(node: TreeItem[Any]): Unit = {
    eventListenerReference = new WeakChangeListener((_, _, unreadEvents) => {
        val currentItem = getTreeItem
        if (currentItem == node) {
//          println("node " + node + " has unread events " + unreadEvents)
          //got to make sure the configured item is the same as when we registered the callback
          //because despite the listener being only weakly stored, it'll only be claimed once the GC passes, hence the
          //configured guard
          if (unreadEvents) {
            if (!getGraphic.getStyleClass.contains("unread-events")) getGraphic.getStyleClass.add("unread-events")
//            println("added style, current styles " + getGraphic.getStyleClass)
          } else {
            getGraphic.getStyleClass.remove("unread-events")
          }
        }
      })
  }
  override protected def updateItem(item: Any, empty: Boolean): Unit = {
    super.updateItem(item, empty)
    
    if (item != null && !empty) {
      getTreeItem match {
        case node: MessageChannelGroup[_] =>
          node match {
            case node @ GuildNode(item) => 
              val serverIcon = Option(item.getIconUrl).map(icon => imageIcon(imagesCache(icon).get)).getOrElse(new Label(item.getName.charAt(0).toUpper.toString))
              val graphic = entry(serverIcon, item.getName, description = "guild - " + item.getRegionRaw).modify(_.getStyleClass add "discord-guild")
              this setGraphic graphic
            
            case dms: DmsNode => setGraphic(new Label("DMs"))
            case chats: GroupChatNode => setGraphic(new Label("Group chats"))
          }
          newEventListenerReference(node)
          node.unreadEvents.addListener(eventListenerReference)
//          println(s"$node has unreadEvents ${node.unreadEvents.get}")
          if (node.unreadEvents.get) {
            if (!getGraphic.getStyleClass.contains("unread-events")) getGraphic.getStyleClass.add("unread-events")
          } else {
            getGraphic.getStyleClass.remove("unread-events")
          }
          
            
        case node @ MessageChannelNode(item) => 
          item match {
            case channel: TextChannel =>
              val res = new TitledPane()
              res.setOnMouseClicked(evt => getTreeView.getSelectionModel.select(getTreeItem))
              res.getStyleClass add "discord-text-channel-titled-pane"
              if (!channel.canTalk) res.getStyleClass add "discord-text-channel-cant-talk"
              val members = channel.getMembers.asScala.map(Right(_): Either[User, Member]).asJava
              res.setGraphic(vbox(new Label(s"🗉 ${channel.getName}"), new Label(members.size + " members")))
              res.setExpanded(false)
              res.setContent(new MembersList(members))
              this setGraphic res
              
            case privateChannel: PrivateChannel =>
              setGraphic(entry(imageIcon(imagesCache(Option(privateChannel.getUser.getAvatarUrl).
                                                     getOrElse(privateChannel.getUser.getDefaultAvatarUrl)).get),
                               privateChannel.getName,
                               Option(privateChannel.getJDA.asClient.getFriend(privateChannel.getUser)).map(_ => "Friend").getOrElse("DM")))
              
            case groupChannel: GroupChannel =>
              val res = new TitledPane()
              res.getStyleClass add "discord-text-channel-titled-pane"
              val groupChatName = Option(groupChannel.getName).filter(_.isEmpty).getOrElse(groupChannel.getUsers.asScala.map(_.getName).mkString(", "))
              res.setGraphic(vbox(new Label(groupChatName, imageIcon(imagesCache(Option(groupChannel.getIconUrl) getOrElse "/groupchats.png").get)),
                                  new Label(groupChannel.getUsers.size + " members")))
              res.setExpanded(false)
              res.setContent(new MembersList(groupChannel.getUsers.asScala.map(Left(_): Either[User, Member]).asJava))
              this setGraphic res
              groupChannel.getIconUrl
              
          }
          newEventListenerReference(node)
          node.unreadEvents.addListener(eventListenerReference)
          if (node.unreadEvents.get) {
            if (!getGraphic.getStyleClass.contains("unread-events")) getGraphic.getStyleClass.add("unread-events")
          } else {
            getGraphic.getStyleClass.remove("unread-events")
          }

        case other => this setGraphic new Label("Unk. type " + other)
      }
    } else {
      setGraphic(null)
    }
  }
  
  
  class MembersList(members: java.util.List[Either[User, Member]]) extends ListView[Either[User, Member]] {
    getStyleClass.add("discord-text-channel-members")
    getItems.addAll(members)
    
    setCellFactory(_ => new ListCell[Either[User, Member]] {
        override protected def updateItem(item: Either[User, Member], empty: Boolean): Unit = {
          super.updateItem(item, empty)
    
          if (item != null && !empty) {
            val user = item.fold(identity, _.getUser)
            val name = item.fold(_.getName, m => Option(m.getNickname).getOrElse(user.getName))
            val color = item.right.toOption.flatMap(m => Option(m.getColor).map(c => Color.rgb(c.getRed, c.getGreen, c.getBlue)))
            
            val res = entry(imageIcon(imagesCache(Option(user.getAvatarUrl).getOrElse(user.getDefaultAvatarUrl)).get),
                            name,
                            if (user.isBot) "BOT" else user.getName + "#" + user.getDiscriminator,
                            color.orNull)
            if (item.right.map(_.isOwner) getOrElse false)
              res.getChildren add new ImageView(guildOwnerIcon).modify(_ setPreserveRatio true, _ setFitWidth textLabelFont.getSize)
            setGraphic(res)
          } else {
            setGraphic(null)
          }
        }
      })
    if (members.size < 4) ccc.util.JfxUtils.showingProperty(this).foreach(showing => if (showing) {
        applyCss()
        this.lookup(".virtual-flow").asInstanceOf[VirtualFlow[_]].setCellCount(members.size)
      })
  }
  
  private def imageIcon(icon: Image) = {
    new StackPane().modify(
      _ setBackground imageBackground(icon),
      _ setStyle "-fx-pref-width: 2.5em; -fx-pref-height: 2.5em")
  }
  private def entry(icon: Node, name: String, description: String, nameColor: Color = null) = {
    val nameAndDescription = vbox(new Label(name).modify(l => if (nameColor != null) l.setTextFill(nameColor)))(spacing = 5)
    Option(description) foreach (d => nameAndDescription.getChildren add new Label(d).modify(_.getStyleClass add "chat-date-label"))
    hbox(icon, nameAndDescription)(spacing = 10, alignment = Pos.TOP_LEFT)
  }
  
  ////////////////////////////////////////////////////////
  // configure drag and drop support
  ////////////////////////////////////////////////////////
  
  setOnDragDetected { evt =>
//    getTreeItem match {
//      
//    }
  }
}
