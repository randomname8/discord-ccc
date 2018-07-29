package discordccc

import better.files._
import ccc._, ccc.util._
import discordccc.model._
import java.time.{Instant, LocalDateTime, ZoneId}
import javafx.application.Application
import javafx.scene.layout._
import javafx.stage._

object EmbedRenderingTest {
  def main(args: Array[String]): Unit = Application.launch(classOf[EmbedRenderingTest])
}
class EmbedRenderingTest extends BaseApplication {
  val sceneRoot = new BorderPane
  val rootUserDataDirectory = (File.home/".discorccc").createDirectories()
  val emojioneDir = (rootUserDataDirectory/"emojione").createDirectories()

  val emojis = EmojiOne.emojiLookup.map(e => e._1 -> new WeakImage(s"file:${emojioneDir}/${e._2.filename}.png"))
  val imagesCache: collection.mutable.Map[String, WeakImage] = new LruMap[String, WeakImage](100).withDefault { k => // cache the most recent images shown in the chat
    val res = new WeakImage(k)
    imagesCache(k) = res
    res
  }
  val users = Map(1l -> null)
  
  val markdownRenderer = new DiscordMarkdownRenderer(getHostServices, imagesCache, _ => None, _ => None, _ => None)
  
  val chatList = new ChatList[Member, Message](getHostServices,
                                               _.nickname,
                                               _.originalContent,
                                               m => LocalDateTime.ofInstant(m.created, ZoneId.systemDefault))
  
  chatList.messageRenderFactory set new MessageRenderer(imagesCache, emojis.mapValues(_.get), markdownRenderer)
  chatList.userNameNodeFactory set MemberRender
  
  sceneRoot setCenter chatList
  
  override def extraInitialize(stage: Stage): Unit = {
    stage.getScene.getStylesheets.addAll("/ccc-theme.css")
    stage.getScene.getStylesheets.addAll("/ccc-discord-theme.css")
    stage setTitle "embed rendering test"
    
    chatList.addEntry(
      Member(1, 1, "Yorha", Seq.empty, 0, false, null),
      imagesCache("https://images-ext-1.discordapp.net/external/SIa2Ai4PzRju_kMP7Qi23-XfDag5CN6Te8FHY9igC70/https/cdn.discordapp.com/avatars/271896884124057600/c33d9a609e3a7d80625331bc17c59e90.png"),
      Message(1, Seq(Content.RichLayout(
          author = Some(Content.RichLayout(title = Some("YoRHa Manual"), url = Some("https://github.com/kvnxiao/yorhabot"), image = Some(Content.InlinedImage("", "https://cdn.discordapp.com/avatars/271896884124057600/c33d9a609e3a7d80625331bc17c59e90.png", "")))),
          color = Some(0xa0a0a0),
          footer = Some(Content.RichLayout(title = Some("34 commands, 4 pages available"))),
          description = Some(Content.Text("Welcome to the user manual for the YoRHa Discord bot! Click the title for the GitHub page.")),
          fields = Seq(
            Content.Field("List of all commands", Content.Text("`?help all`: lists all available command aliases."), true),
            Content.Field("View commands per page", Content.Text("`?help {number}`: lists commands on the specified page (e.g. `?help 1`)."), true),
            Content.Field("View a specific command", Content.Text("`?help <command alias>`: view a specific command's details (e.g. `?help ?ping`)."), true),
            Content.Field("View a specific command", Content.Text("`?help <command alias>`: view a specific command's details (e.g. `?help ?ping`)."), true),
            Content.Field("View a specific command", Content.Text("`?help <command alias>`: view a specific command's details (e.g. `?help ?ping`)."), false),
            Content.Field("View a specific command", Content.Text("`?help <command alias>`: view a specific command's details (e.g. `?help ?ping`)."), true),
            ),
          thumbnail = Some(Content.InlinedImage("amazing picture of the day", "https://images-ext-2.discordapp.net/external/30TssbgJS-DyZ5LWYrQIjDGp7x5t8uyWiHEfoEioFx4/https/epic.gsfc.nasa.gov/archive/natural/2018/06/30/png/epic_1b_20180630005516.png?width=450&height=450", "", width = 400, height = 400))
          )), Instant.now, None, Seq.empty, 0, 1, null))
    chatList.addEntry(
      Member(1, 1, "Yorha", Seq.empty, 0, false, null),
      imagesCache("https://images-ext-1.discordapp.net/external/SIa2Ai4PzRju_kMP7Qi23-XfDag5CN6Te8FHY9igC70/https/cdn.discordapp.com/avatars/271896884124057600/c33d9a609e3a7d80625331bc17c59e90.png"),
      Message(1, Seq(Content.Text(""""
but as an inexperienced professional programmer, its great
since I've been able to interact with so many people with way more experience than me as a result
(since normally I'm kinda shy irl, so i wouldn't have talked to nearly as many people)
@Techtony96 ye
""")), Instant.now, None, Seq.empty, 0, 1, null))
  }
}
