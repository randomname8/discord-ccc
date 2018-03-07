package discordccc

import language.implicitConversions

import javafx.beans.Observable
import javafx.beans.binding.BooleanBinding
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.value.ObservableBooleanValue
import javafx.scene.control.TreeItem
import net.dv8tion.jda.client.entities.Group
import net.dv8tion.jda.core.entities.{Guild, TextChannel, MessageChannel, PrivateChannel}
import scala.collection.JavaConverters._

/**
 * Define classes that wrap some JDA model classes in order to expose events such as new member in a guild.
 * This is necessary so that the UI can react to JDA events
 */
object JavafxJdaWrapper {
  
  
  trait MessageChannelGroup[MC <: MessageChannel] extends TreeItem[Any] {
    protected object unreadEventsBinding extends BooleanBinding {
      def bind(o: Observable) = super.bind(o)
      def unbind(o: Observable) = super.unbind(o)
      override def computeValue() = getChildren.asScala.collect { case n: MessageChannelNode => n.unreadEvents.get }.foldLeft(false)(_ || _)
    }
    val unreadEvents: ObservableBooleanValue = unreadEventsBinding
    def textChannelModified(channel: MC, added: Boolean): Unit = {
      if (added) {
        val node = new MessageChannelNode(channel)
        getChildren add node
        unreadEventsBinding.bind(node.unreadEvents)
      } else {
        getChildren.asScala.find(_.getValue == channel) foreach {
          case n: MessageChannelNode =>
            unreadEventsBinding.unbind(n.unreadEvents)
            getChildren.remove(n)
        }
      }
    }
  }
  case class MessageChannelNode(messageChannel: MessageChannel) extends TreeItem[Any](messageChannel) {
    val unreadEvents = new SimpleBooleanProperty(this, "newMessagesAvailable", false)
    override def toString = s"MessageChannelNode($messageChannel)"
  }
  
  case class GuildNode(guild: Guild) extends TreeItem[Any](guild) with MessageChannelGroup[TextChannel] {
    guild.getTextChannels.get(0).getPosition
    guild.getTextChannels forEach (c => textChannelModified(c, true))
    override def toString = s"GuildNode($guild)"
  }
  implicit def guildNode2Guild(gn: GuildNode) = gn.guild
  implicit def textChannelNode2TextChannel(tn: MessageChannelNode) = tn.messageChannel
  
  class DmsNode extends TreeItem[Any]("dms") with MessageChannelGroup[PrivateChannel] {
    override def toString = "DmsNode"
  }
  class GroupChatNode extends TreeItem[Any]("group chats") with MessageChannelGroup[Group] {
    override def toString = "GroupChatNode"
  }
}
