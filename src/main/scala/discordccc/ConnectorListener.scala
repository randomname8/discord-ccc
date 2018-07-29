package discordccc

import discordccc.model.{Connector}, Connector._
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
      Platform.runLater { () => 
        if (selectedChannel != null && message.channelId == selectedChannel.id && message.connector == selectedChannel.connector) {
          addMessage(message)
        } else {
          connector.getChannel(message.channelId) foreach { c =>
            if (c.dmUserId.nonEmpty) dmChannelUpdated(c)
            else if (c.serverId.isEmpty) groupChannelUpdated(c)
            else channelUpdated(c, connector.getServer(c.serverId.get).get)
          }
        }
      }
  }
}
