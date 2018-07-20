package discordccc

import discordccc.model.{Member, User, Connector}, Connector._
import javafx.application.Platform

trait ConnectorListener { self: DiscordChat =>
  
  val connectorListener: PartialFunction[Event, Any] = {
    
    case ServerEvent(server, Created, _) => Platform.runLater(() => addServer(server))
      
    case ChannelEvent(channel, Created, connector) => Platform.runLater { () => 
        (channel.serverId, channel.dmUserId) match {
          case (Some(serverId), _) => channelModifiedInServer(channel, connector.getServer(serverId).get, true)
          case (_, Some(dmUser)) => dmChannelModified(channel, true)
          case (_, _) => groupChannelModified(channel, true)
        }
      }
      
    case MessageEvent(message, Created, connector) =>
      val selectedChannel = selectedMessageChannel.get
      if (selectedChannel != null && message.channelId == selectedChannel.id && message.connector == selectedChannel.connector) {
        Platform.runLater(() => addMessage(message))
      }
//        val user = connector.getUser(message.authorId).getOrElse(User(0, "unk.", false, "non existent user?", None, false, null))
//        val member = connector.getMember(message.authorId, message.channelId).getOrElse( 
//          Member(user.id, selectedChannel.serverId.getOrElse(0), user.name, Seq.empty, 0, false, null))
//        Platform.runLater { () => chatList.addEntry(member, imagesCache(user.imageUrl.getOrElse("/red-questionmark.png")), message) }
//      }
  }
}
