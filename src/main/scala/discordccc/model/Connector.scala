package discordccc
package model

import enumeratum.values.{ByteEnum, ByteEnumEntry}

/**
 * Connectors represent integration to a protocol such as XMPP or Discord.
 * You should typically implement your connector in a trait, and then register it in the ConnectorRegistry
 * by defining an entry mixing in your trait.
 */
sealed trait Connector extends ByteEnumEntry {
  def canRead(member: Member, channel: Channel, server: Server): Boolean
  
  def serverMetadataClass: Class[_]
  def channelMetadataClass: Class[_]
  def memberMetadataClass: Class[_]
  def userMetadataClass: Class[_]
}
trait ConnectorEntity {
  def connector: Connector
  def metadata: Any
}

object ConnectorRegistry extends ByteEnum[Connector] {
  val values = findValues
  object DiscordConnector extends Connector with connector.DiscordConnector { val value = 1 }
  /* object YourEntry extends Connector with YourImpl { val value = N } */
}