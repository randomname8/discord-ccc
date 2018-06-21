package discordccc

import ccc._
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
//    discordClient.asClient.getGroups forEach (group => groupChatNode.textChannelModified(group, true))
//    discordClient.getPrivateChannels forEach (channel => dmsNode.textChannelModified(channel, true))
//    discordClient.getGuilds forEach (g => entries add GuildNode(g))
    
    navigationTree setCellFactory { _ => new ServersAccessTreeCell(imagesCache, null, _ => null, emojis("👑").get) }
    
    navigationTree.getSelectionModel.selectedItemProperty foreach (item => if (item != null) item match {
        case ChannelNode(channel) => selectedMessageChannel set channel
        case other =>
      }
    )
    
//   old code
    { 
  
//    def findGuildNode(g: Guild) = navigationTree.getRoot.getChildren.asScala.collectFirst { case n@GuildNode(`g`) => n }
//    discordClient.addEventListener(new EventListener {
//        val removeFromParent: TreeItem[Any] => Unit = n => n.getParent.getChildren.remove(n)
//    
//        override def onEvent(event: Event): Unit = Platform.runLater { () =>
//          event match {
//            case evt: GuildJoinEvent => navigationTree.getRoot.getChildren add GuildNode(evt.getGuild)
//            case evt: GuildLeaveEvent => findGuildNode(evt.getGuild) foreach removeFromParent
//              
//            case evt: TextChannelCreateEvent => findGuildNode(evt.getGuild) foreach (_.textChannelModified(evt.getChannel, true))
//            case evt: TextChannelDeleteEvent => findGuildNode(evt.getGuild) foreach (_.textChannelModified(evt.getChannel, false))
//              
//            case evt: PrivateChannelCreateEvent => dmsNode.textChannelModified(evt.getPrivateChannel, true)
//            case evt: PrivateChannelDeleteEvent => dmsNode.textChannelModified(evt.getPrivateChannel, false)
//              
//            case evt: GroupJoinEvent => groupChatNode.textChannelModified(evt.getGroup, true)
//            case evt: GroupLeaveEvent => groupChatNode.textChannelModified(evt.getGroup, false)
//              
//            case evt: GenericMessageEvent =>
//              if (selectedMessageChannel.get != evt.getChannel) {
////                println("checking for unread events " + evt.getClass.getSimpleName + " - " + evt.getChannel)
//                evt match {
//                  case evt: GenericMessageEvent if evt.getChannelType == ChannelType.PRIVATE => dmsNode.getChildren.asScala.
//                    collectFirst { case n@MessageChannelNode(c) if c == evt.getChannel => n } foreach (_.unreadEvents set true)
//                  case evt: GenericMessageEvent if evt.getChannelType == ChannelType.GROUP => groupChatNode.getChildren.asScala.
//                    collectFirst { case n@MessageChannelNode(c) if c == evt.getChannel => n } foreach (_.unreadEvents set true)
//                  case evt: GenericMessageEvent if evt.getChannelType == ChannelType.TEXT => 
//                    val textChannel = evt.getChannel.asInstanceOf[TextChannel]
//                    findGuildNode(textChannel.getGuild).flatMap(_.getChildren.asScala.collectFirst {
//                        case n@MessageChannelNode(`textChannel`) => n}) foreach (_.unreadEvents set true)
//                  
//                  case _ =>
//                }
//              }
//
//            case _ =>
//          }
//        }
//      })
//    
//    selectedMessageChannel foreach { channel =>
//      channel.getType match {
//        case ChannelType.PRIVATE =>
//          dmsNode.getChildren.asScala.collectFirst { case n@MessageChannelNode(c) if c == channel => n } foreach (_.unreadEvents set false)
//        case ChannelType.GROUP =>
//          groupChatNode.getChildren.asScala.collectFirst { case n@MessageChannelNode(c) if c == channel => n } foreach (_.unreadEvents set false)
//        case _ =>
//          val textChannel = channel.asInstanceOf[TextChannel]
//          findGuildNode(textChannel.getGuild).flatMap(_.getChildren.asScala.collectFirst {
//              case n@MessageChannelNode(`textChannel`) => n}) foreach (_.unreadEvents set false)
//      }
//    }
    
    }    
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
