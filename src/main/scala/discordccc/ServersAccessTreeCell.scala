package discordccc

import ccc._, ccc.util._
import discordccc.model._
import javafx.scene.Node
import javafx.scene.control.{Label, TreeCell, ListView, ListCell, TreeItem, TitledPane}
import javafx.beans.value.WeakChangeListener
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.text.Font
import scala.collection.JavaConverters._
import ChatTreeItems._

object ServersAccessTreeCell {
  val textLabelFont = Font.font(Font.getDefault.getName, Font.getDefault.getSize * 1.5)
  val unreadableChannelColor = Color.RED
  val defaultAvatarUrl = "https://discordapp.com/assets/dd4dbc0016779df1378e7812eabaa04d.png"
  type UserId = Long
}
import ServersAccessTreeCell._
class ServersAccessTreeCell(
  val imagesCache: collection.Map[String, WeakImage],
  val membersFetcher: Channel => IndexedSeq[Member Either User],
  val usersLookup: (UserId, Channel) => User,
  val guildOwnerIcon: Image) extends TreeCell[Any] {
  
  
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
        case node: ChannelGroup =>
          node match {
            case node @ ServerNode(item) =>
              val serverIcon = item.imageUrl.map(icon => imageIcon(imagesCache(icon))).getOrElse(new Label(item.name.charAt(0).toUpper.toString))
              val graphic = entry(serverIcon, item.name, description = "server - " + item.location).modify(_.getStyleClass add "discord-guild")
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
          
          
        case node @ ChannelNode(channel) if channel.dmUserId.isEmpty => 
          val res = new TitledPane()
          res.getStyleClass add "discord-text-channel-titled-pane"
          if (!channel.canTalk) res.getStyleClass add "discord-text-channel-cant-talk"
          res.setGraphic(new Label(s"🗉 ${channel.name}"))
          res.setCollapsible(false)
          res.setExpanded(false)
          
          res.setOnMouseClicked { evt =>
            getTreeView.getSelectionModel.select(getTreeItem)
            if (evt.getClickCount == 2) {
              res.setExpanded(!res.isExpanded)
              res.setContent(null)
              if (res.isExpanded) {
                
                val l0 = System.currentTimeMillis
                val mmbrs = membersFetcher(channel)
                println(s"Time fetching users ${System.currentTimeMillis - l0}ms")
                res.setContent(new MembersList(channel, mmbrs))
              }
            }
          }
          
          this setGraphic res
          
          newEventListenerReference(node)
          node.unreadEvents.addListener(eventListenerReference)
          if (node.unreadEvents.get) {
            if (!getGraphic.getStyleClass.contains("unread-events")) getGraphic.getStyleClass.add("unread-events")
          } else {
            getGraphic.getStyleClass.remove("unread-events")
          }
          
        case node @ ChannelNode(channel) =>
          val user = usersLookup(channel.dmUserId.get, channel)
          val icon = user.imageUrl.map(icon => imageIcon(imagesCache(icon))).getOrElse(new Label(user.name.charAt(0).toUpper.toString))
          setGraphic(entry(icon, channel.name, if (user.friend) "Friend" else "DM"))
          
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
  
  
  private type ListEntry = Either[Member, User]
//  private case class MemberListEntry(member: Either[User, Member]) extends ListEntry
//  private case object LoadMoreListEntry extends ListEntry
  private class MembersList(channel: Channel, members: IndexedSeq[Member Either User]) extends ListView[ListEntry] {
    getStyleClass.add("discord-text-channel-members")

    setItems(FXCollections.observableList(members.asJava))
    
    
    setCellFactory(_ => new ListCell[ListEntry] {
        override protected def updateItem(item: ListEntry, empty: Boolean): Unit = {
          super.updateItem(item, empty)
    
          if (item != null && !empty) {
            val user = item.fold(m => usersLookup(m.userId, channel), identity)
            val name = item.fold(_.nickname, _.name)
            val color = item.left.toOption.flatMap(m => Option(m.color).filter(_ != 0).map(c => Color.rgb((c >>> 16) & 0xff, (c >>> 8) & 0xff, c & 0xff)))
            
            val res = entry(imageIcon(imagesCache(user.imageUrl.getOrElse(defaultAvatarUrl))),
                            name, if (user.bot) "BOT" else user.name + "#" + user.extra, color.orNull)
            if (item.left.toOption.map(_.isOwner) getOrElse false)
              res.getChildren add new ImageView(guildOwnerIcon).modify(_ setPreserveRatio true, _ setFitWidth textLabelFont.getSize)

            setGraphic(res)
              
          } else {
            setGraphic(null)
          }
        }
      })
  }
  
  private def imageIcon(icon: WeakImage) = {
    val res = new StackPane()
    icon.onRetrieve(i => res setBackground imageBackground(i))
    res setStyle "-fx-pref-width: 2.5em; -fx-pref-height: 2.5em"
    res
  }
  private def entry(icon: Node, name: String, description: String, nameColor: Color = null) = {
    val nameAndDescription = vbox(new Label(name).modify(l => if (nameColor != null) l.setTextFill(nameColor)))(spacing = 5)
    Option(description) foreach (d => nameAndDescription.getChildren add new Label(d).modify(_.getStyleClass add "chat-date-label"))
    hbox(icon, nameAndDescription)(spacing = 10, alignment = Pos.TOP_LEFT)
  }
}
