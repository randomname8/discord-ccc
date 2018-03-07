package discordccc

import ccc._
import javafx.application.Platform
import javafx.scene.control.TreeItem
import net.dv8tion.jda.client.events.group.{GroupJoinEvent, GroupLeaveEvent}
import net.dv8tion.jda.core.entities.{Guild, ChannelType, TextChannel}
import net.dv8tion.jda.core.events.Event
import net.dv8tion.jda.core.events.channel.priv.{PrivateChannelCreateEvent, PrivateChannelDeleteEvent}
import net.dv8tion.jda.core.events.channel.text.{TextChannelCreateEvent, TextChannelDeleteEvent}
import net.dv8tion.jda.core.events.guild.{GuildJoinEvent, GuildLeaveEvent}
import net.dv8tion.jda.core.events.message.GenericMessageEvent
import net.dv8tion.jda.core.hooks.EventListener
import scala.collection.JavaConverters._
import JavafxJdaWrapper._

trait DiscordNavigationTree { self: CccDiscord =>
  def configureDiscordTree(): Unit = {
    val root = new TreeItem[Any]()
    accessTree setRoot root
    val entries = root.getChildren
    val dmsNode = new DmsNode()
    entries add dmsNode
    val groupChatNode = new GroupChatNode()
    entries add groupChatNode
    discordClient.asClient.getGroups forEach (group => groupChatNode.textChannelModified(group, true))
    discordClient.getPrivateChannels forEach (channel => dmsNode.textChannelModified(channel, true))
    discordClient.getGuilds forEach (g => entries add GuildNode(g))
    
    accessTree setCellFactory { _ => new DiscordAccessTreeCell(imagesCache, emojis("👑").get) }
    
    accessTree.getSelectionModel.selectedItemProperty foreach (item => if (item != null) item match {
        case MessageChannelNode(channel) => selectedMessageChannel set channel
        case other =>
      }
    )

    def findGuildNode(g: Guild) = accessTree.getRoot.getChildren.asScala.collectFirst { case n@GuildNode(`g`) => n }
    discordClient.addEventListener(new EventListener {
        val removeFromParent: TreeItem[Any] => Unit = n => n.getParent.getChildren.remove(n)
    
        override def onEvent(event: Event): Unit = Platform.runLater { () =>
          event match {
            case evt: GuildJoinEvent => accessTree.getRoot.getChildren add GuildNode(evt.getGuild)
            case evt: GuildLeaveEvent => findGuildNode(evt.getGuild) foreach removeFromParent
              
            case evt: TextChannelCreateEvent => findGuildNode(evt.getGuild) foreach (_.textChannelModified(evt.getChannel, true))
            case evt: TextChannelDeleteEvent => findGuildNode(evt.getGuild) foreach (_.textChannelModified(evt.getChannel, false))
              
            case evt: PrivateChannelCreateEvent => dmsNode.textChannelModified(evt.getPrivateChannel, true)
            case evt: PrivateChannelDeleteEvent => dmsNode.textChannelModified(evt.getPrivateChannel, false)
              
            case evt: GroupJoinEvent => groupChatNode.textChannelModified(evt.getGroup, true)
            case evt: GroupLeaveEvent => groupChatNode.textChannelModified(evt.getGroup, false)
              
            case evt: GenericMessageEvent =>
              if (selectedMessageChannel.get != evt.getChannel) {
//                println("checking for unread events " + evt.getClass.getSimpleName + " - " + evt.getChannel)
                evt match {
                  case evt: GenericMessageEvent if evt.getChannelType == ChannelType.PRIVATE => dmsNode.getChildren.asScala.
                    collectFirst { case n@MessageChannelNode(c) if c == evt.getChannel => n } foreach (_.unreadEvents set true)
                  case evt: GenericMessageEvent if evt.getChannelType == ChannelType.GROUP => groupChatNode.getChildren.asScala.
                    collectFirst { case n@MessageChannelNode(c) if c == evt.getChannel => n } foreach (_.unreadEvents set true)
                  case evt: GenericMessageEvent if evt.getChannelType == ChannelType.TEXT => 
                    val textChannel = evt.getChannel.asInstanceOf[TextChannel]
                    findGuildNode(textChannel.getGuild).flatMap(_.getChildren.asScala.collectFirst {
                        case n@MessageChannelNode(`textChannel`) => n}) foreach (_.unreadEvents set true)
                  
                  case _ =>
                }
              }

            case _ =>
          }
        }
      })
    
    selectedMessageChannel foreach { channel =>
      channel.getType match {
        case ChannelType.PRIVATE =>
          dmsNode.getChildren.asScala.collectFirst { case n@MessageChannelNode(c) if c == channel => n } foreach (_.unreadEvents set false)
        case ChannelType.GROUP =>
          groupChatNode.getChildren.asScala.collectFirst { case n@MessageChannelNode(c) if c == channel => n } foreach (_.unreadEvents set false)
        case _ =>
          val textChannel = channel.asInstanceOf[TextChannel]
          findGuildNode(textChannel.getGuild).flatMap(_.getChildren.asScala.collectFirst {
              case n@MessageChannelNode(`textChannel`) => n}) foreach (_.unreadEvents set false)
      }
    }
  }
}
