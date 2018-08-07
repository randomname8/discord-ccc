package discordccc

import ccc._, MarkdownRenderer._
import ccc.util.WeakImage
import discordccc.model._
import javafx.application.HostServices
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.control.ProgressIndicator
import javafx.scene.image.Image
import javafx.scene.layout.Pane
import javafx.scene.layout.Region
import javafx.scene.text.Font
import org.commonmark.{node => md}
import org.commonmark.parser.{Parser => MdParser}
import regex._
import DiscordMarkdownRenderer._
import scala.util.matching.Regex.Match

class DiscordMarkdownRenderer(hostServices: HostServices, imagesCache: collection.Map[String, WeakImage],
                              userProvider: Long => Option[User],
                              channelProvider: Long => Option[Channel],
                              customEmojiProvider: Long => Option[String]) extends MarkdownRenderer(
  MdParser.builder.extensions(java.util.Arrays.asList(
      org.commonmark.ext.autolink.AutolinkExtension.create(),
      org.commonmark.ext.gfm.strikethrough.StrikethroughExtension.create())).
  customDelimiterProcessor(MarkdownExtensions.InsDelimiterProcessor).
  postProcessor(UserMentionsProcessor).
  postProcessor(ChannelMentionsProcessor).
  postProcessor(CustomEmojisProcessor).build()
) {
  val nodeFactory = new DiscordMarkdownNodeFactory(hostServices, imagesCache)
  
  def render(text: String, emojiProvider: Map[String, Image])(context: MarkdownRenderer.RenderContext): Seq[Node] =
    render(text, emojiProvider, nodeFactory)(context)
  
  override val customNodeSupport: PartialFunction[md.CustomNode, (Map[String, Image], NodeFactory, RenderContext) => (Node, Inlined)] = {
    case UserMention(user) => (emojis, _, context) => 
      (userProvider(user) match {
          case Some(user) => new Label(user.name).modify(_.getStyleClass add "user-mention")
//          case Some(Left(member)) => new Label(member.nickname).modify(_.getStyleClass add "mention")
          case None => new Label(s"Unk.user($user)").modify(_.getStyleClass add "unknown-user-mention")
        }) -> true
      
    case ChannelMention(channel) => (_, _, context) =>
      (channelProvider(channel) match {
          case Some(channel) => new Label("#" + channel.name).modify(_.getStyleClass add "channel-mention")
          case _ => new Label(s"#unk.channel($channel)").modify(_.getStyleClass add "unknown-channel-mention")
        }) -> true
      
    case CustomEmoji(name, emoji, animated) => (_, _, context) =>
      (customEmojiProvider(emoji) match {
          case Some(emoji) => 
//            imagesCache(if (!animated) emoji else emoji.replace(".png", ".gif")).onRetrieve(im => )
            DiscordMarkdownRenderer.loadingIcon(imagesCache(if (!animated) emoji else emoji.replace(".png", ".gif")))(i =>
              nodeFactory.mkEmoji(context)(name, i))
//            nodeFactory.mkEmoji(context)(name, imagesCache(if (!animated) emoji else emoji.replace(".png", ".gif")).get)
          case _ => new Label(s"<:$name:$emoji>").modify(_.getStyleClass add "unknown-emoji")
        }) -> true
  }
}
object DiscordMarkdownRenderer {
  
  import org.commonmark.parser.PostProcessor

  trait RegexProcessor extends PostProcessor {
    def pattern: Gregex[_]
    
    def toNode(match1: Match): md.Node
    
    override def process(node: md.Node): md.Node = {
      node.accept(new md.AbstractVisitor {
          override def visit(t: md.Text) = {
            pattern.findAllMatchIn(t.getLiteral).to[List] match {
              case mentions if mentions.nonEmpty =>
                var nodePointer: md.Node = t
                def append(n: md.Node) = {
                  nodePointer = {
                    nodePointer.insertAfter(n)
                    n
                  }
                }
                val head :: tail = mentions.sliding(2).to[List]
                Option(head.head.before.toString) filter (_.nonEmpty) foreach (t => append(new md.Text(t)))
                append(toNode(head.head))
                for (List(prev, mention) <- tail) {
                  append(new md.Text(t.getLiteral.substring(prev.end, mention.start)))
                  append(toNode(mention))
                }
                mentions.lastOption.map(_.after.toString) filter (_.nonEmpty) foreach (t => append(new md.Text(t)))
                t.unlink()
                
              case _=>
            }
          }
        })
      node
    }
  }
  
  object UserMentionsProcessor extends RegexProcessor {
    val pattern = gr"""<@!?(\d+)>"""
    def toNode(mention: Match) = UserMention(mention.group(1).toLong)
  }
  case class UserMention(user: Long) extends md.CustomNode
  object ChannelMentionsProcessor extends RegexProcessor {
    val pattern = gr"""<#!?(\d+)>"""
    def toNode(mention: Match) = ChannelMention(mention.group(1).toLong)
  }
  case class ChannelMention(channel: Long) extends md.CustomNode
  object CustomEmojisProcessor extends RegexProcessor {
    val pattern = gr"""<(a)?:([^:]+):(\d+)>""" 
    def toNode(mention: Match) = CustomEmoji(mention.group(2), mention.group(3).toLong, mention.group(1) != null)
  }
  case class CustomEmoji(name: String, id: Long, animated: Boolean) extends md.CustomNode
  
  def loadingIcon(image: WeakImage)(onceLoaded: Image => Node): Node = {
    val imageContainer = new Pane()
    imageContainer.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
    imageContainer.getChildren.add(new ProgressIndicator())
    image.onRetrieve { image =>
      imageContainer.getChildren.clear()
      imageContainer.getChildren.add(onceLoaded(image))
    }
    imageContainer
  }
}

class DiscordMarkdownNodeFactory(hostServices: HostServices, imagesCache: collection.Map[String, WeakImage]) extends DefaultMarkdownNodeFactory(hostServices, imagesCache) {

  override def desiredEmojiHeight = Font.getDefault.getSize * 2.2
//  override def mkCodeBlock(context: MarkdownRenderer.RenderContext)(language: Option[String], code: String) = {
//    super.mkCodeBlock(context)(language, code.replace("\\\n", "\n"))
//  }
  override def mkInlineContent(context: MarkdownRenderer.RenderContext)(title: String, url: String, altText: String, width: Double, height: Double) = {
    altText match {
      case "emoji" => DiscordMarkdownRenderer.loadingIcon(imagesCache(url))(i => super.mkEmoji(context)(title, i))
      case _ => super.mkInlineContent(context)(title, url, altText, width, height)
    }
  }
}

