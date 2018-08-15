package discordccc

import ccc._
import discordccc.model.{Connector}, Connector._
import javafx.application.Platform
import javafx.beans.value.ChangeListener

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
      
    case MessageCreatedEvent(message, connector) =>
      val selectedChannel = selectedMessageChannel.get
      Platform.runLater { () => 
        if (selectedChannel != null && message.channelId == selectedChannel.id && message.connector == selectedChannel.connector) {
          addMessage(message)
          
          if (sceneRoot.getScene.getWindow.isFocused) {
            connector.markAsRead(message).failed.foreach(_.printStackTrace)(JavafxExecutionContext)
          } else {
            lazy val focusOneoff: ChangeListener[java.lang.Boolean] = (prop, _, v) => if (v) {
              //as soon as we regain focus, mark as read
              prop.removeListener(focusOneoff)
              connector.markAsRead(message).failed.foreach(_.printStackTrace)(JavafxExecutionContext)
            }
            sceneRoot.getScene.getWindow.focusedProperty.addListener(focusOneoff)
          }
          
        } else {
          connector.getChannel(message.channelId).foreach(channelUpdated(_, connector))
        }
      }
            
    case MessageUpdatedEvent(message, connector) =>
      val selectedChannel = selectedMessageChannel.get
      Platform.runLater { () => 
        if (selectedChannel != null && message.channelId == selectedChannel.id && message.connector == selectedChannel.connector) {
          updateMessage(message)
        } else {
          connector.getChannel(message.channelId).foreach(channelUpdated(_, connector))
        }
      }

    case ChannelUnreadEvent(channel, connector) =>
      Platform.runLater { () =>
        connector.getChannel(channel.id).foreach(channelUpdated(_, connector))
      }
  }
}
