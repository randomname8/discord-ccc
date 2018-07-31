package discordccc

import ccc._
import discordccc.model._
import javafx.scene.control.TreeItem
import scala.collection.JavaConverters._
import ChatTreeItems._

trait NavigationTree { self: DiscordChat =>
  
  private[this] lazy val root = new TreeItem[Any]()
  private[this] lazy val dmsNode = new DmsNode()
  private[this] lazy val groupChatNode = new GroupChatNode()
  
  def configureNavigationTree(): Unit = {
    
    navigationTree setRoot root
    val entries = root.getChildren
    entries add dmsNode
    entries add groupChatNode
    
    navigationTree setCellFactory { _ => new ServersAccessTreeCell(imagesCache, c => c.connector.getMembers(c), (id, channel) => channel.connector.getUser(id).getOrElse(
        User(0, "Unk.", false, s"Member $id not found", None, false, null)), emojis("👑").get) }
    
    navigationTree.getSelectionModel.selectedItemProperty foreach (item => if (item != null) item match {
        case ChannelNode(channel) => 
          selectedMessageChannel set channel
          val node = if (channel.dmUserId.isDefined) nodeFor(channel, dmsNode)
          else if (channel.serverId.isEmpty) nodeFor(channel, groupChatNode)
          else channel.connector.getServer(channel.serverId.get).flatMap(s => findServer(s).getChildren.asScala.collectFirst { case n @ ChannelNode(`channel`) => n })
          node.foreach(_.unreadEvents set false)
        case other =>
      }
    )
  }
  
  private def missingServer(server: Server): Nothing = throw new IllegalArgumentException(s"Server $server was not added!")
  private def findServer(server: Server): ServerNode = root.getChildren.asScala.collectFirst { case n @ ServerNode(`server`) => n}.getOrElse(missingServer(server))
  def addServer(server: Server): Unit = root.getChildren.add(new ServerNode(server))
  def removeServer(server: Server): Unit = root.getChildren remove findServer(server)
  def channelModifiedInServer(channel: Channel, toServer: Server, added: Boolean): Unit = {
    findServer(toServer).channelModified(channel, added)
  }
  def channelUpdated(channel: Channel, inServer: Server): Unit = {
    findServer(inServer).getChildren.asScala.collectFirst { case n @ ChannelNode(`channel`) => n }.foreach(_.unreadEvents set true)
  }
  
  def dmChannelModified(channel: Channel, added: Boolean): Unit = dmsNode.channelModified(channel, added)
  def groupChannelModified(channel: Channel, added: Boolean): Unit = groupChatNode.channelModified(channel, added)
  private def nodeFor(channel: Channel, parent: ChannelGroup): Option[ChannelNode] = parent.getChildren.asScala.collectFirst { case n @ ChannelNode(`channel`) => n }
  def dmChannelUpdated(channel: Channel): Unit = nodeFor(channel, dmsNode).foreach(_.unreadEvents set true)
  def groupChannelUpdated(channel: Channel): Unit = nodeFor(channel, groupChatNode).foreach(_.unreadEvents set true)
}
