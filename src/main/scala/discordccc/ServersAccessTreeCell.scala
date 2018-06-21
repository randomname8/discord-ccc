package discordccc

import ccc._
import javafx.scene.Node
import javafx.scene.control.skin.VirtualFlow
import javafx.scene.control.{Label, TreeCell, ListView, ListCell, TreeItem, TitledPane, ProgressIndicator}
import javafx.beans.value.WeakChangeListener
import javafx.geometry.Pos
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.text.Font
import scala.concurrent.Future
import ChatTreeItems._

object ServersAccessTreeCell {
  val textLabelFont = Font.font(Font.getDefault.getName, Font.getDefault.getSize * 1.5)
  val unreadableChannelColor = Color.RED
  val defaultAvatarUrl = "https://discordapp.com/assets/dd4dbc0016779df1378e7812eabaa04d.png"
  type ChannelId = String
  type UserId = String
}
import ServersAccessTreeCell._
class ServersAccessTreeCell(
  val imagesCache: collection.Map[String, util.WeakImage],
  val membersFetcher: ChannelId => Future[Seq[Either[User, Member]]],
  val usersLookup: UserId => User,
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
              val serverIcon = item.imageUrl.map(icon => imageIcon(imagesCache(icon).get)).getOrElse(new Label(item.name.charAt(0).toUpper.toString))
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
                res.setContent(new ProgressIndicator())
                import JavafxExecutionContext.context
                membersFetcher(channel.id).foreach(users => res.setContent(new MembersList(users)))
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
          val user = usersLookup(channel.dmUserId.get)
          val icon = user.imageUrl.map(icon => imageIcon(imagesCache(icon).get)).getOrElse(new Label(user.name.charAt(0).toUpper.toString))
          setGraphic(entry(icon, channel.name, if (user.friend) "Friend" else "DM"))
          
        case other => this setGraphic new Label("Unk. type " + other)
      }
    } else {
      setGraphic(null)
    }
  }
  
  
  class MembersList(members: Seq[Either[User, Member]]) extends ListView[Either[User, Member]] {
    getStyleClass.add("discord-text-channel-members")
    getItems.addAll(members:_*)
    
    setCellFactory(_ => new ListCell[Either[User, Member]] {
        override protected def updateItem(item: Either[User, Member], empty: Boolean): Unit = {
          super.updateItem(item, empty)
    
          if (item != null && !empty) {
            val user = item.fold(identity, m => usersLookup(m.userId))
            val name = item.fold(_.name, _.nickname)
            val color = item.right.toOption.flatMap(m => Option(m.color).filter(_ != 0).map(c => Color.rgb((c >>> 16) & 0xff, (c >>> 8) & 0xff, c & 0xff)))
            
            val res = entry(imageIcon(imagesCache(user.imageUrl.getOrElse(defaultAvatarUrl)).get),
                            name, if (user.bot) "BOT" else user.name + "#" + user.extra, color.orNull)

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
}
