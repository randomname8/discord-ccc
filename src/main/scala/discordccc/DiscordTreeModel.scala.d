package discordccc

import javafx.scene.control.TreeItem
import net.dv8tion.jda.core.entities.Guild
import net.dv8tion.jda.core.entities.TextChannel
import scala.collection.JavaConverters._

object DiscordTreeModel {

  abstract class DynamicTreeItem[T] private[DiscordTreeModel](value: T)(childrenF: T => Seq[TreeItem[Any]]) extends TreeItem[Any](value) {
    //when the node is collapsed, discard calculated children
    addEventHandler[TreeItem.TreeModificationEvent[Any]](TreeItem.branchCollapsedEvent[Any], evt => super.getChildren.clear())
    override def getChildren = {
      val children = super.getChildren
      if (!children.isEmpty) children
      else {
        children.addAll(childrenF(value):_*)
        children
      }
    }
    def lastVisibleChildren = if (isExpanded) Some(super.getChildren) else None
    
    override def isLeaf = false
  }
  case class GuildNode(guild: Guild) extends DynamicTreeItem(guild)(_.getTextChannels.asScala.map(e => TextChannelNode(e): TreeItem[Any]))
  
  case class TextChannelNode(textChannel: TextChannel) extends DynamicTreeItem(textChannel)(_.getMembers.asScala.map(e => new TreeItem[Any](e)))
}
