package discordccc

import net.dv8tion.jda.core.entities.Message

object DiscordMarkdown {

  /**
   * Apply necessary transformations to the Discord supported markdown to transform it to standard markdown.
   */
  def adaptToMarkdown(message: Message): String = {
    var res = message.getContentDisplay.trim.replace(" ```", "\n```").replace("\n", "\\\n")
    message.getEmotes forEach { emote =>
      res = res.replace(s":${emote.getName}:", s"""![emoji](${emote.getImageUrl} "${emote.getName}")""")
    }
    res
  }
}
