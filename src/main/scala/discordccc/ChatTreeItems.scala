package discordccc

import discordccc.model._
import javafx.beans.Observable
import javafx.beans.binding.BooleanBinding
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.value.ObservableBooleanValue
import javafx.scene.control.TreeItem
import scala.collection.JavaConverters._

object ChatTreeItems {

  trait ChannelGroup extends TreeItem[Any] {
    protected object unreadEventsBinding extends BooleanBinding {
      def bind(o: Observable) = super.bind(o)
      def unbind(o: Observable) = super.unbind(o)
      override def computeValue() = getChildren.asScala.collect { case n: ChannelNode => n.unreadEvents.get }.foldLeft(false)(_ || _)
    }
    val unreadEvents: ObservableBooleanValue = unreadEventsBinding
    def channelModified(channel: Channel, added: Boolean): Unit = {
      if (added) {
        val node = new ChannelNode(channel)
        getChildren add node
        unreadEventsBinding.bind(node.unreadEvents)
      } else {
        getChildren.asScala.find(_.getValue == channel) foreach {
          case n: ChannelNode =>
            unreadEventsBinding.unbind(n.unreadEvents)
            getChildren.remove(n)
        }
      }
    }
  }
  case class ChannelNode(channel: Channel) extends TreeItem[Any](channel) {
    val unreadEvents = new SimpleBooleanProperty(this, "newMessagesAvailable", false)
    override def toString = s"ChannelNode($channel)"
    override def isLeaf = true
  }
  
  case class ServerNode(server: Server) extends TreeItem[Any](server) with ChannelGroup {
    override def toString = s"ServerNode($server)"
  }
//  implicit def serverNode2Server(n: ServerNode) = n.server
//  implicit def textChannelNode2TextChannel(tn: MessageChannelNode) = tn.messageChannel
  
  class DmsNode extends TreeItem[Any]("dms") with ChannelGroup {
    override def toString = "DmsNode"
  }
  class GroupChatNode extends TreeItem[Any]("group chats") with ChannelGroup {
    override def toString = "GroupChatNode"
  }
  
  
//  abstract class DynamicTreeItem[T] private[ChatTreeModel](value: T)(childrenF: T => Seq[TreeItem[Any]]) extends TreeItem[Any](value) {
//    //when the node is collapsed, discard calculated children
//    addEventHandler[TreeItem.TreeModificationEvent[Any]](TreeItem.branchCollapsedEvent[Any], evt => super.getChildren.clear())
//    override def getChildren = {
//      val children = super.getChildren
//      if (!children.isEmpty) children
//      else {
//        children.addAll(childrenF(value):_*)
//        children
//      }
//    }
//    def lastVisibleChildren = if (isExpanded) Some(super.getChildren) else None
//    
//    override def isLeaf = false
//  }
//  case class ServerNode(server: Server, channels: Seq[Channel]) extends DynamicTreeItem(server)(_ => channels.map(e => ChannelNode(e): TreeItem[Any]))
//  
//  case class ChannelNode(channel: Channel) extends DynamicTreeItem(channel)(_.members.map(e => new TreeItem[Any](e)))
}
